
-- |
-- Module     : Simulation.Aivika.Experiment.TimeSeriesView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'TimeSeriesView' that saves the time series
-- charts as the PNG files.
--

module Simulation.Aivika.Experiment.TimeSeriesView
       (TimeSeriesView(..), 
        defaultTimeSeriesView) where

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
import Simulation.Aivika.Experiment.Chart (colourisePlotLines)

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue

-- | Defines the 'View' that saves the time series charts
-- in the PNG files.
data TimeSeriesView =
  TimeSeriesView { timeSeriesTitle       :: String,
                   -- ^ This is a title used in HTML.
                   timeSeriesDescription :: String,
                   -- ^ This is a description used in HTML.
                   timeSeriesWidth       :: Int,
                   -- ^ The width of the chart.
                   timeSeriesHeight      :: Int,
                   -- ^ The height of the chart.
                   timeSeriesFileName    :: FileName,
                   -- ^ It defines the file name for each PNG file. 
                   -- It may include special variables @$TITLE@, 
                   -- @$RUN_INDEX@ and @$RUN_COUNT@.
                   --
                   -- An example is
                   --
                   -- @
                   --   timeSeriesFileName = UniqueFileName \"$TITLE - $RUN_INDEX\", \".png\"
                   -- @
                   timeSeriesPredicate   :: Dynamics Bool,
                   -- ^ It specifies the predicate that defines
                   -- when we plot data in the chart.
                   timeSeries      :: [Either String String],
                   -- ^ It contains the labels of data plotted
                   -- on the chart.
                   timeSeriesPlotTitle :: String,
                   -- ^ This is a title used in the chart when
                   -- simulating a single run. It may include 
                   -- special variable @$TITLE@.
                   --
                   -- An example is
                   --
                   -- @
                   --   timeSeriesPlotTitle = \"$TITLE\"
                   -- @
                   timeSeriesRunPlotTitle :: String,
                   -- ^ The run title for the chart. It is used 
                   -- when simulating multiple runs and it may 
                   -- include special variables @$RUN_INDEX@, 
                   -- @$RUN_COUNT@ and @$PLOT_TITLE@.
                   --
                   -- An example is 
                   --
                   -- @
                   --   timeSeriesRunPlotTitle = \"$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                   -- @
                   timeSeriesPlotLines :: [PlotLines Double Double ->
                                           PlotLines Double Double],
                   -- ^ Probably, an infinite sequence of plot 
                   -- transformations based on which the plot
                   -- is constructed for each series. Generally,
                   -- it must not coincide with a sequence of 
                   -- labels as one label may denote a whole list 
                   -- or an array of data providers.
                   --
                   -- Here you can define a colour or style of
                   -- the plot lines.
                   timeSeriesBottomAxis :: LayoutAxis Double ->
                                           LayoutAxis Double,
                   -- ^ A transformation of the bottom axis, 
                   -- after title @time@ is added.
                   timeSeriesLayout :: Layout1 Double Double ->
                                       Layout1 Double Double
                   -- ^ A transformation of the plot layout, 
                   -- where you can redefine the axes, for example.
                 }
  
-- | The default time series view.  
defaultTimeSeriesView :: TimeSeriesView
defaultTimeSeriesView = 
  TimeSeriesView { timeSeriesTitle       = "Time Series",
                   timeSeriesDescription = "It shows the Time Series chart(s).",
                   timeSeriesWidth       = 640,
                   timeSeriesHeight      = 480,
                   timeSeriesFileName    = UniqueFileName "$TITLE - $RUN_INDEX" ".png",
                   timeSeriesPredicate   = return True,
                   timeSeries            = [], 
                   timeSeriesPlotTitle   = "$TITLE",
                   timeSeriesRunPlotTitle = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                   timeSeriesPlotLines   = colourisePlotLines,
                   timeSeriesBottomAxis  = id,
                   timeSeriesLayout      = id }

instance View TimeSeriesView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newTimeSeries v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateTimeSeries st,
                               reporterTOCHtml    = timeSeriesTOCHtml st,
                               reporterHtml       = timeSeriesHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data TimeSeriesViewState =
  TimeSeriesViewState { timeSeriesView       :: TimeSeriesView,
                        timeSeriesExperiment :: Experiment,
                        timeSeriesDir        :: FilePath, 
                        timeSeriesMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newTimeSeries :: TimeSeriesView -> Experiment -> FilePath -> IO TimeSeriesViewState
newTimeSeries view exp dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i -> 
       resolveFileName (Just dir) (timeSeriesFileName view) $
       M.fromList [("$TITLE", timeSeriesTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return TimeSeriesViewState { timeSeriesView       = view,
                                  timeSeriesExperiment = exp,
                                  timeSeriesDir          = dir, 
                                  timeSeriesMap          = m }
       
-- | Plot the time series chart during the simulation.
simulateTimeSeries :: TimeSeriesViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateTimeSeries st expdata =
  do let protolabels = timeSeries $ timeSeriesView st
         labels = flip map protolabels $ either id id
         providers = experimentSeriesProviders expdata labels
         input =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateTimeSeries"
             Just input -> input
         n = experimentRunCount $ timeSeriesExperiment st
         width = timeSeriesWidth $ timeSeriesView st
         height = timeSeriesHeight $ timeSeriesView st
         predicate = timeSeriesPredicate $ timeSeriesView st
         plotLines = timeSeriesPlotLines $ timeSeriesView st
         plotBottomAxis = timeSeriesBottomAxis $ timeSeriesView st
         plotLayout = timeSeriesLayout $ timeSeriesView st
     i <- liftSimulation simulationIndex
     let file = fromJust $ M.lookup (i - 1) (timeSeriesMap st)
         title = timeSeriesTitle $ timeSeriesView st
         plotTitle = 
           replace "$TITLE" title
           (timeSeriesPlotTitle $ timeSeriesView st)
         runPlotTitle =
           if n == 1
           then plotTitle
           else replace "$RUN_INDEX" (show i) $
                replace "$RUN_COUNT" (show n) $
                replace "$PLOT_TITLE" plotTitle
                (timeSeriesRunPlotTitle $ timeSeriesView st)
     hs <- forM (zip providers input) $ \(provider, input) ->
       let transform () =
             do x <- predicate
                if x then input else return (1/0)  -- the infinite values will be ignored then
       in newSignalHistoryThrough (experimentQueue expdata) $
          mapSignalM transform $
          experimentMixedSignal expdata [provider]
     return $
       do ps <- forM (zip3 hs providers plotLines) $ \(h, provider, plotLines) ->
            do (ts, xs) <- readSignalHistory h 
               return $
                 toPlot $
                 plotLines $
                 plot_lines_values ^= filterPlotLinesValues (zip (elems ts) (elems xs)) $
                 plot_lines_title ^= providerName provider $
                 defaultPlotLines
          let ps' = flip map (zip ps protolabels) $ \(p, label) ->
                case label of
                  Left _  -> Left p
                  Right _ -> Right p
              axis  = plotBottomAxis $
                      laxis_title ^= "time" $
                      defaultLayoutAxis
              chart = plotLayout $
                      layout1_bottom_axis ^= axis $
                      layout1_title ^= runPlotTitle $
                      layout1_plots ^= ps' $
                      defaultLayout1
          liftIO $ 
            do renderableToPNGFile (toRenderable chart) width height file
               when (experimentVerbose $ timeSeriesExperiment st) $
                 putStr "Generated file " >> putStrLn file
     
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [(Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) .
  divideBy (\(t, x) -> isNaN x || isInfinite x)

-- | Get the HTML code.     
timeSeriesHtml :: TimeSeriesViewState -> Int -> HtmlWriter ()     
timeSeriesHtml st index =
  let n = experimentRunCount $ timeSeriesExperiment st
  in if n == 1
     then timeSeriesHtmlSingle st index
     else timeSeriesHtmlMultiple st index
     
-- | Get the HTML code for a single run.
timeSeriesHtmlSingle :: TimeSeriesViewState -> Int -> HtmlWriter ()
timeSeriesHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (timeSeriesMap st)
     writeHtmlParagraph $
       writeHtmlImage (makeRelative (timeSeriesDir st) f)

-- | Get the HTML code for multiple runs.
timeSeriesHtmlMultiple :: TimeSeriesViewState -> Int -> HtmlWriter ()
timeSeriesHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ timeSeriesExperiment st
     forM_ [0..(n - 1)] $ \i ->
       let f = fromJust $ M.lookup i (timeSeriesMap st)
       in writeHtmlParagraph $
          writeHtmlImage (makeRelative (timeSeriesDir st) f)

header :: TimeSeriesViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (timeSeriesTitle $ timeSeriesView st)
     let description = timeSeriesDescription $ timeSeriesView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
timeSeriesTOCHtml :: TimeSeriesViewState -> Int -> HtmlWriter ()
timeSeriesTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (timeSeriesTitle $ timeSeriesView st)
