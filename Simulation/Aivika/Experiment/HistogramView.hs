
-- |
-- Module     : Simulation.Aivika.Experiment.HistogramView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'HistogramView' that saves the histogram
-- in the PNG files by all integration time points for each 
-- simulation run.
--
-- Use 'FinalHistogramView' if you need a histogram
-- calculated only in final time points for all runs
-- simultaneously.
--

module Simulation.Aivika.Experiment.HistogramView
       (HistogramView(..), 
        defaultHistogramView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Array

import Data.Accessor

import System.IO
import System.FilePath

import Data.String.Utils (replace)

import Graphics.Rendering.Chart

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (divideBy)
import Simulation.Aivika.Experiment.Chart (colourisePlotBars)
import Simulation.Aivika.Experiment.Histogram

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue

-- | Defines the 'View' that saves the histogram in 
-- the PNG files by all integration time points for 
-- each simulation run.
--
-- Use 'FinalHistogramView' if you need a histogram
-- calculated only in final time points for all runs
-- simultaneously.
data HistogramView =
  HistogramView { histogramTitle       :: String,
                  -- ^ This is a title used in HTML and histogram.
                  histogramRunTitle    :: String,
                  -- ^ The run title for the view. It is used 
                  -- when simulating multiple runs and it may 
                  -- include special variables @$RUN_INDEX@, 
                  -- @$RUN_COUNT@ and @$TITLE@.
                  --
                  -- An example is 
                  --
                  -- @
                  --   histogramRunTitle = \"$TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                  -- @
                  histogramDescription :: String,
                  -- ^ This is a description in the HTML.
                  histogramWidth       :: Int,
                  -- ^ The width of the histogram.
                  histogramHeight      :: Int,
                  -- ^ The height of the histogram.
                  histogramFileName    :: FileName,
                  -- ^ It defines the file name for each PNG file. 
                  -- It may include special variables @$TITLE@, 
                  -- @$RUN_INDEX@ and @$RUN_COUNT@.
                  --
                  -- An example is
                  --
                  -- @
                  --   histogramFileName = UniqueFileName \"$TITLE - $RUN_INDEX\", \".png\"
                  -- @
                  histogramPredicate   :: Dynamics Bool,
                  -- ^ It specifies the predicate that defines
                  -- when we count data when plotting the histogram.
                  histogramBuild       :: [[Double]] -> Histogram, 
                  -- ^ Builds a histogram by the specified list of 
                  -- data series.
                  histogramSeries      :: [String],
                  -- ^ It contains the labels of data for which
                  -- the histogram is plotted.
                  histogramPlotBars :: PlotBars Double Double ->
                                       PlotBars Double Double,
                  -- ^ A transformation based on which the plot bar
                  -- is constructed for the series. 
                  --
                  -- Here you can define a colour or style of
                  -- of the plot bars.
                  histogramLayout :: Layout1 Double Double ->
                                     Layout1 Double Double
                  -- ^ A transformation of the plot layout, 
                  -- where you can redefine the axes, for example.
                }
  
-- | The default histogram view.  
defaultHistogramView :: HistogramView
defaultHistogramView = 
  HistogramView { histogramTitle       = "Histogram",
                  histogramRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  histogramDescription = [],
                  histogramWidth       = 640,
                  histogramHeight      = 480,
                  histogramFileName    = UniqueFileName "$TITLE - $RUN_INDEX" ".png",
                  histogramPredicate   = return True,
                  histogramBuild       = histogram binSturges,
                  histogramSeries      = [], 
                  histogramPlotBars    = colourisePlotBars,
                  histogramLayout      = id }

instance View HistogramView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newHistogram v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateHistogram st,
                               reporterTOCHtml    = histogramTOCHtml st,
                               reporterHtml       = histogramHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data HistogramViewState =
  HistogramViewState { histogramView       :: HistogramView,
                       histogramExperiment :: Experiment,
                       histogramDir        :: FilePath, 
                       histogramMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newHistogram :: HistogramView -> Experiment -> FilePath -> IO HistogramViewState
newHistogram view exp dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i -> 
       resolveFileName (Just dir) (histogramFileName view) $
       M.fromList [("$TITLE", histogramTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     let m = M.fromList $ zip [0..(n - 1)] fs
     return HistogramViewState { histogramView       = view,
                                 histogramExperiment = exp,
                                 histogramDir        = dir, 
                                 histogramMap        = m }
       
-- | Plot the histogram during the simulation.
simulateHistogram :: HistogramViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateHistogram st expdata =
  do let labels = histogramSeries $ histogramView st
         providers = experimentSeriesProviders expdata labels
         names = map providerName providers
         input =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateHistogram"
             Just input -> input
         n = experimentRunCount $ histogramExperiment st
         width = histogramWidth $ histogramView st
         height = histogramHeight $ histogramView st
         predicate = histogramPredicate $ histogramView st
         bars = histogramPlotBars $ histogramView st
         layout = histogramLayout $ histogramView st
         build = histogramBuild $ histogramView st
     i <- liftSimulation simulationIndex
     let file = fromJust $ M.lookup (i - 1) (histogramMap st)
         title =
           if n == 1
           then histogramTitle $ histogramView st
           else replace "$RUN_INDEX" (show i) $
                replace "$RUN_COUNT" (show n) $
                replace "$TITLE" (histogramTitle $ histogramView st)
                (histogramRunTitle $ histogramView st)
     hs <- forM (zip providers input) $ \(provider, input) ->
       newSignalHistoryThrough (experimentQueue expdata) $
       mapSignalM (const input) $
       filterSignalM (const predicate) $
       experimentSignalInIntegTimes expdata
     return $
       do xs <- forM hs readSignalHistory
          let zs = histogramToBars . filterHistogram . build $ 
                   map (filterData . elems . snd) xs
              p  = plotBars $
                   bars $
                   plot_bars_values ^= zs $
                   plot_bars_titles ^= names $
                   defaultPlotBars
              chart = layout $
                      layout1_title ^= title $
                      layout1_plots ^= [Left p] $
                      defaultLayout1
          liftIO $ 
            do renderableToPNGFile (toRenderable chart) width height file
               when (experimentVerbose $ histogramExperiment st) $
                 putStr "Generated file " >> putStrLn file
     
-- | Remove the NaN and inifity values.     
filterData :: [Double] -> [Double]
filterData = filter (\x -> not $ isNaN x || isInfinite x)
     
-- | Remove the NaN and inifity values.     
filterHistogram :: [(Double, a)] -> [(Double, a)]
filterHistogram = filter (\(x, _) -> not $ isNaN x || isInfinite x)
  
-- | Convert a histogram to the bars.
histogramToBars :: [(Double, [Int])] -> [(Double, [Double])]
histogramToBars = map $ \(x, ns) -> (x, map fromIntegral ns)

-- | Get the HTML code.     
histogramHtml :: HistogramViewState -> Int -> HtmlWriter ()     
histogramHtml st index =
  let n = experimentRunCount $ histogramExperiment st
  in if n == 1
     then histogramHtmlSingle st index
     else histogramHtmlMultiple st index
     
-- | Get the HTML code for a single run.
histogramHtmlSingle :: HistogramViewState -> Int -> HtmlWriter ()
histogramHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (histogramMap st)
     writeHtmlParagraph $
       writeHtmlImage (makeRelative (histogramDir st) f)

-- | Get the HTML code for multiple runs.
histogramHtmlMultiple :: HistogramViewState -> Int -> HtmlWriter ()
histogramHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ histogramExperiment st
     forM_ [0..(n - 1)] $ \i ->
       let f = fromJust $ M.lookup i (histogramMap st)
       in writeHtmlParagraph $
          writeHtmlImage (makeRelative (histogramDir st) f)

header :: HistogramViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (histogramTitle $ histogramView st)
     let description = histogramDescription $ histogramView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
histogramTOCHtml :: HistogramViewState -> Int -> HtmlWriter ()
histogramTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (histogramTitle $ histogramView st)