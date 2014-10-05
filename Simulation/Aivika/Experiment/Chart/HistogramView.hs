
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.HistogramView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines 'HistogramView' that plots the histogram
-- collecting statistics in all integration time points and does
-- it for every simulation run separately.
--

module Simulation.Aivika.Experiment.Chart.HistogramView
       (HistogramView(..), 
        defaultHistogramView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Monoid
import Data.Array
import Data.Default.Class

import System.IO
import System.FilePath

import Graphics.Rendering.Chart

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotBars)
import Simulation.Aivika.Experiment.Histogram

-- | Defines the 'View' that plots the histogram collecting statistics
-- for all integration time points but for each simulation run separately.
data HistogramView =
  HistogramView { histogramTitle       :: String,
                  -- ^ This is a title used in HTML.
                  histogramDescription :: String,
                  -- ^ This is a description used in HTML.
                  histogramWidth       :: Int,
                  -- ^ The width of the histogram.
                  histogramHeight      :: Int,
                  -- ^ The height of the histogram.
                  histogramFileName    :: ExperimentFilePath,
                  -- ^ It defines the file name with optional extension for each image to be saved.
                  -- It may include special variables @$TITLE@, @$RUN_INDEX@ and @$RUN_COUNT@.
                  --
                  -- An example is
                  --
                  -- @
                  --   histogramFileName = UniqueFilePath \"$TITLE - $RUN_INDEX\"
                  -- @
                  histogramPredicate   :: Event Bool,
                  -- ^ It specifies the predicate that defines
                  -- when we count data when plotting the histogram.
                  histogramBuild       :: [[Double]] -> Histogram, 
                  -- ^ Builds a histogram by the specified list of 
                  -- data series.
                  histogramTransform   :: ResultTransform,
                  -- ^ The transform applied to the results before receiving series.
                  histogramSeries      :: ResultTransform, 
                  -- ^ It defines the series to be plotted on the histogram.
                  histogramPlotTitle   :: String,
                  -- ^ This is a title used in the histogram when
                  -- simulating a single run. It may include 
                  -- special variable @$TITLE@.
                  --
                  -- An example is
                  --
                  -- @
                  --   histogramPlotTitle = \"$TITLE\"
                  -- @
                  histogramRunPlotTitle :: String,
                  -- ^ The run title for the histogram. It is used 
                  -- when simulating multiple runs and it may 
                  -- include special variables @$RUN_INDEX@, 
                  -- @$RUN_COUNT@ and @$PLOT_TITLE@.
                  --
                  -- An example is 
                  --
                  -- @
                  --   histogramRunPlotTitle = \"$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                  -- @
                  histogramPlotBars :: PlotBars Double Double ->
                                       PlotBars Double Double,
                  -- ^ A transformation based on which the plot bar
                  -- is constructed for the series. 
                  --
                  -- Here you can define a colour or style of
                  -- the plot bars.
                  histogramLayout :: Layout Double Double ->
                                     Layout Double Double
                  -- ^ A transformation of the plot layout, 
                  -- where you can redefine the axes, for example.
                }
  
-- | The default histogram view.  
defaultHistogramView :: HistogramView
defaultHistogramView = 
  HistogramView { histogramTitle       = "Histogram",
                  histogramDescription = "It shows the histogram(s) by data gathered in the integration time points.",
                  histogramWidth       = 640,
                  histogramHeight      = 480,
                  histogramFileName    = UniqueFilePath "$TITLE - $RUN_INDEX",
                  histogramPredicate   = return True,
                  histogramBuild       = histogram binSturges,
                  histogramTransform   = id,
                  histogramSeries      = mempty, 
                  histogramPlotTitle   = "$TITLE",
                  histogramRunPlotTitle = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  histogramPlotBars    = colourisePlotBars,
                  histogramLayout      = id }

instance WebPageCharting r => ExperimentView HistogramView r WebPageWriter where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newHistogram v exp renderer dir
             let writer =
                   WebPageWriter { reporterWriteTOCHtml = histogramTOCHtml st,
                                   reporterWriteHtml    = histogramHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateHistogram st,
                                         reporterRequest    = writer }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data HistogramViewState r =
  HistogramViewState { histogramView       :: HistogramView,
                       histogramExperiment :: Experiment,
                       histogramRenderer   :: r,
                       histogramDir        :: FilePath, 
                       histogramMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newHistogram :: WebPageCharting r => HistogramView -> Experiment -> r -> FilePath -> IO (HistogramViewState r)
newHistogram view exp renderer dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i ->
       resolveFilePath dir $
       mapFilePath (flip replaceExtension $ renderableChartExtension renderer) $
       expandFilePath (histogramFileName view) $
       M.fromList [("$TITLE", histogramTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return HistogramViewState { histogramView       = view,
                                 histogramExperiment = exp,
                                 histogramRenderer   = renderer,
                                 histogramDir        = dir, 
                                 histogramMap        = m }
       
-- | Plot the histogram during the simulation.
simulateHistogram :: WebPageCharting r => HistogramViewState r -> ExperimentData -> Event DisposableEvent
simulateHistogram st expdata =
  do let view    = histogramView st
         rs      = histogramSeries view $
                   histogramTransform view $
                   experimentResults expdata
         exts    = extractDoubleListResults rs
         names   = map resultExtractName exts
         signals = experimentPredefinedSignals expdata
         n = experimentRunCount $ histogramExperiment st
         build   = histogramBuild view
         width   = histogramWidth view
         height  = histogramHeight view
         predicate  = histogramPredicate view
         title   = histogramTitle view
         plotTitle  = histogramPlotTitle view
         runPlotTitle = histogramRunPlotTitle view
         bars       = histogramPlotBars view
         layout     = histogramLayout view
         renderer   = histogramRenderer st
     i <- liftParameter simulationIndex
     let file = fromJust $ M.lookup (i - 1) (histogramMap st)
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         runPlotTitle' =
           if n == 1
           then plotTitle'
           else replace "$RUN_INDEX" (show i) $
                replace "$RUN_COUNT" (show n) $
                replace "$PLOT_TITLE" plotTitle'
                runPlotTitle
     hs <- forM exts $ \ext ->
       newSignalHistory $
       mapSignalM (const $ resultExtractData ext) $
       filterSignalM (const predicate) $
       resultSignalInIntegTimes signals
     return $
       DisposableEvent $
       do xs <- forM hs readSignalHistory
          let zs = histogramToBars . filterHistogram . build $ 
                   map (filterData . concat . elems . snd) xs
              p  = plotBars $
                   bars $
                   plot_bars_values .~ zs $
                   plot_bars_titles .~ names $
                   def
              updateAxes =
                if null zs
                then let v = AxisVisibility True False False
                     in \l -> layout_top_axis_visibility .~ v $
                              layout_bottom_axis_visibility .~ v $
                              layout_left_axis_visibility .~ v $
                              layout_right_axis_visibility .~ v $
                              l
                else id
              chart = layout . updateAxes $
                      layout_title .~ runPlotTitle' $
                      layout_plots .~ [p] $
                      def
          liftIO $
            do renderChart renderer (width, height) file (toRenderable chart)
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
histogramHtml :: HistogramViewState r -> Int -> HtmlWriter ()     
histogramHtml st index =
  let n = experimentRunCount $ histogramExperiment st
  in if n == 1
     then histogramHtmlSingle st index
     else histogramHtmlMultiple st index
     
-- | Get the HTML code for a single run.
histogramHtmlSingle :: HistogramViewState r -> Int -> HtmlWriter ()
histogramHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (histogramMap st)
     writeHtmlParagraph $
       writeHtmlImage (makeRelative (histogramDir st) f)

-- | Get the HTML code for multiple runs.
histogramHtmlMultiple :: HistogramViewState r -> Int -> HtmlWriter ()
histogramHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ histogramExperiment st
     forM_ [0..(n - 1)] $ \i ->
       let f = fromJust $ M.lookup i (histogramMap st)
       in writeHtmlParagraph $
          writeHtmlImage (makeRelative (histogramDir st) f)

header :: HistogramViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (histogramTitle $ histogramView st)
     let description = histogramDescription $ histogramView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
histogramTOCHtml :: HistogramViewState r -> Int -> HtmlWriter ()
histogramTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (histogramTitle $ histogramView st)
