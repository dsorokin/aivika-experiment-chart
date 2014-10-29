
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.TimeSeriesView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'TimeSeriesView' that plots the time series charts.
--

module Simulation.Aivika.Experiment.Chart.TimeSeriesView
       (TimeSeriesView(..), 
        defaultTimeSeriesView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Array
import Data.List
import Data.Monoid
import Data.Default.Class

import System.IO
import System.FilePath

import Graphics.Rendering.Chart

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines)

-- | Defines the 'View' that plots the time series charts.
data TimeSeriesView =
  TimeSeriesView { timeSeriesTitle       :: String,
                   -- ^ This is a title used in HTML.
                   timeSeriesDescription :: String,
                   -- ^ This is a description used in HTML.
                   timeSeriesWidth       :: Int,
                   -- ^ The width of the chart.
                   timeSeriesHeight      :: Int,
                   -- ^ The height of the chart.
                   timeSeriesFileName    :: ExperimentFilePath,
                   -- ^ It defines the file name with optional extension for each image to be saved.
                   -- It may include special variables @$TITLE@, @$RUN_INDEX@ and @$RUN_COUNT@.
                   --
                   -- An example is
                   --
                   -- @
                   --   timeSeriesFileName = UniqueFilePath \"$TITLE - $RUN_INDEX\"
                   -- @
                   timeSeriesPredicate   :: Event Bool,
                   -- ^ It specifies the predicate that defines
                   -- when we plot data in the chart.
                   timeSeriesTransform    :: ResultTransform,
                   -- ^ The transform applied to the results before receiving series.
                   timeSeriesLeftYSeries  :: ResultTransform, 
                   -- ^ It defines the series plotted basing on the left Y axis.
                   timeSeriesRightYSeries :: ResultTransform, 
                   -- ^ It defines the series plotted basing on the right Y axis.
                   timeSeriesPlotTitle    :: String,
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
                   timeSeriesLayout :: LayoutLR Double Double Double ->
                                       LayoutLR Double Double Double
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
                   timeSeriesFileName    = UniqueFilePath "TimeSeries($RUN_INDEX)",
                   timeSeriesPredicate   = return True,
                   timeSeriesTransform   = id,
                   timeSeriesLeftYSeries  = const mempty,
                   timeSeriesRightYSeries = const mempty,
                   timeSeriesPlotTitle    = "$TITLE",
                   timeSeriesRunPlotTitle = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                   timeSeriesPlotLines   = colourisePlotLines,
                   timeSeriesBottomAxis  = id,
                   timeSeriesLayout      = id }

instance WebPageCharting r => ExperimentView TimeSeriesView r WebPageWriter where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newTimeSeries v exp renderer dir
             let writer =
                   WebPageWriter { reporterWriteTOCHtml = timeSeriesTOCHtml st,
                                   reporterWriteHtml    = timeSeriesHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateTimeSeries st,
                                         reporterRequest    = writer }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data TimeSeriesViewState r =
  TimeSeriesViewState { timeSeriesView       :: TimeSeriesView,
                        timeSeriesExperiment :: Experiment,
                        timeSeriesRenderer   :: r,
                        timeSeriesDir        :: FilePath, 
                        timeSeriesMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newTimeSeries :: WebPageCharting r => TimeSeriesView -> Experiment -> r -> FilePath -> ExperimentWriter (TimeSeriesViewState r)
newTimeSeries view exp renderer dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i ->
       resolveFilePath dir $
       mapFilePath (flip replaceExtension $ renderableChartExtension renderer) $
       expandFilePath (timeSeriesFileName view) $
       M.fromList [("$TITLE", timeSeriesTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     liftIO $ forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return TimeSeriesViewState { timeSeriesView       = view,
                                  timeSeriesExperiment = exp,
                                  timeSeriesRenderer   = renderer,
                                  timeSeriesDir        = dir, 
                                  timeSeriesMap        = m }
       
-- | Plot the time series chart within simulation.
simulateTimeSeries :: WebPageCharting r => TimeSeriesViewState r -> ExperimentData -> Event DisposableEvent
simulateTimeSeries st expdata =
  do let view    = timeSeriesView st
         rs1     = timeSeriesLeftYSeries view $
                   timeSeriesTransform view $
                   experimentResults expdata
         rs2     = timeSeriesRightYSeries view $
                   timeSeriesTransform view $
                   experimentResults expdata
         exts1   = extractDoubleResults rs1
         exts2   = extractDoubleResults rs2
         signals = experimentPredefinedSignals expdata
         n = experimentRunCount $ timeSeriesExperiment st
         width   = timeSeriesWidth view
         height  = timeSeriesHeight view
         predicate  = timeSeriesPredicate view
         title   = timeSeriesTitle view
         plotTitle  = timeSeriesPlotTitle view
         runPlotTitle = timeSeriesRunPlotTitle view
         plotLines  = timeSeriesPlotLines view
         plotBottomAxis = timeSeriesBottomAxis view
         plotLayout = timeSeriesLayout view
         renderer   = timeSeriesRenderer st
     i <- liftParameter simulationIndex
     let file  = fromJust $ M.lookup (i - 1) (timeSeriesMap st)
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
         inputHistory exts =
           forM exts $ \ext ->
           let transform () =
                 do x <- predicate
                    if x
                      then resultExtractData ext
                      else return (1/0)  -- the infinite values will be ignored then
           in newSignalHistory $
              mapSignalM transform $
              pureResultSignal signals $
              resultExtractSignal ext
     hs1 <- inputHistory exts1
     hs2 <- inputHistory exts2
     return $
       DisposableEvent $
       do let plots hs exts plotLineTails =
                do ps <-
                     forM (zip3 hs exts (head plotLineTails)) $
                     \(h, ext, plotLines) ->
                     do (ts, xs) <- readSignalHistory h 
                        return $
                          toPlot $
                          plotLines $
                          plot_lines_values .~ filterPlotLinesValues (zip (elems ts) (elems xs)) $
                          plot_lines_title .~ resultExtractName ext $
                          def
                   return (ps, drop (length hs) plotLineTails)
          (ps1, plotLineTails) <- plots hs1 exts1 (tails plotLines)
          (ps2, plotLineTails) <- plots hs2 exts2 plotLineTails
          let ps1' = map Left ps1
              ps2' = map Right ps2
              ps'  = ps1' ++ ps2'
              axis = plotBottomAxis $
                     laxis_title .~ "time" $
                     def
              updateLeftAxis =
                if null ps1
                then layoutlr_left_axis_visibility .~ AxisVisibility False False False
                else id
              updateRightAxis =
                if null ps2
                then layoutlr_right_axis_visibility .~ AxisVisibility False False False
                else id
              chart = plotLayout . updateLeftAxis . updateRightAxis $
                      layoutlr_x_axis .~ axis $
                      layoutlr_title .~ runPlotTitle' $
                      layoutlr_plots .~ ps' $
                      def
          liftIO $
            do renderChart renderer (width, height) file (toRenderable chart)
               when (experimentVerbose $ timeSeriesExperiment st) $
                 putStr "Generated file " >> putStrLn file
     
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [(Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) .
  divideBy (\(t, x) -> isNaN x || isInfinite x)

-- | Get the HTML code.     
timeSeriesHtml :: TimeSeriesViewState r -> Int -> HtmlWriter ()     
timeSeriesHtml st index =
  let n = experimentRunCount $ timeSeriesExperiment st
  in if n == 1
     then timeSeriesHtmlSingle st index
     else timeSeriesHtmlMultiple st index
     
-- | Get the HTML code for a single run.
timeSeriesHtmlSingle :: TimeSeriesViewState r -> Int -> HtmlWriter ()
timeSeriesHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (timeSeriesMap st)
     writeHtmlParagraph $
       writeHtmlImage (makeRelative (timeSeriesDir st) f)

-- | Get the HTML code for multiple runs.
timeSeriesHtmlMultiple :: TimeSeriesViewState r -> Int -> HtmlWriter ()
timeSeriesHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ timeSeriesExperiment st
     forM_ [0..(n - 1)] $ \i ->
       let f = fromJust $ M.lookup i (timeSeriesMap st)
       in writeHtmlParagraph $
          writeHtmlImage (makeRelative (timeSeriesDir st) f)

header :: TimeSeriesViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (timeSeriesTitle $ timeSeriesView st)
     let description = timeSeriesDescription $ timeSeriesView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
timeSeriesTOCHtml :: TimeSeriesViewState r -> Int -> HtmlWriter ()
timeSeriesTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (timeSeriesTitle $ timeSeriesView st)
