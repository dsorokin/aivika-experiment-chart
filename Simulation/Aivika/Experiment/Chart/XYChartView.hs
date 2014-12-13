
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.XYChartView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines 'XYChartView' that plots the XY charts.
--

module Simulation.Aivika.Experiment.Chart.XYChartView
       (XYChartView(..), 
        defaultXYChartView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Array
import Data.Monoid
import Data.List
import Data.Default.Class

import System.IO
import System.FilePath

import Graphics.Rendering.Chart

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines)

-- | Defines the 'View' that plots the XY charts.
data XYChartView =
  XYChartView { xyChartTitle       :: String,
                -- ^ This is a title used in HTML.
                xyChartDescription :: String,
                -- ^ This is a description used in HTML.
                xyChartWidth       :: Int,
                -- ^ The width of the chart.
                xyChartHeight      :: Int,
                -- ^ The height of the chart.
                xyChartFileName    :: ExperimentFilePath,
                -- ^ It defines the file name with optional extension for each image to be saved.
                -- It may include special variables @$TITLE@, @$RUN_INDEX@ and @$RUN_COUNT@.
                --
                -- An example is
                --
                -- @
                --   xyChartFileName = UniqueFilePath \"$TITLE - $RUN_INDEX\"
                -- @
                xyChartPredicate   :: Event Bool,
                -- ^ It specifies the predicate that defines
                -- when we plot data in the chart.
                xyChartTransform   :: ResultTransform,
                -- ^ The transform applied to the results before receiving series.
                xyChartXSeries     :: ResultTransform,
                -- ^ This is the X series.
                --
                -- You must define it, because it is 'mempty' 
                -- by default. Also it must return exactly
                -- one 'ResultExtract' item when calling
                -- function 'extractDoubleResults' by the specified
                -- result set.
                xyChartLeftYSeries  :: ResultTransform, 
                -- ^ It defines the series plotted basing on the left Y axis.
                xyChartRightYSeries :: ResultTransform, 
                -- ^ It defines the series plotted basing on the right Y axis.
                xyChartPlotTitle   :: String,
                -- ^ This is a title used in the chart when
                -- simulating a single run. It may include 
                -- special variable @$TITLE@.
                --
                -- An example is
                --
                -- @
                --   xyChartPlotTitle = \"$TITLE\"
                -- @
                xyChartRunPlotTitle :: String,
                -- ^ The run title for the chart. It is used 
                -- when simulating multiple runs and it may 
                -- include special variables @$RUN_INDEX@, 
                -- @$RUN_COUNT@ and @$PLOT_TITLE@.
                --
                -- An example is 
                --
                -- @
                --   xyChartRunPlotTitle = \"$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                -- @
                xyChartPlotLines :: [PlotLines Double Double ->
                                     PlotLines Double Double],
                -- ^ Probably, an infinite sequence of plot 
                -- transformations based on which the plot
                -- is constructed for each Y series. Generally,
                -- it may not coincide with a sequence of 
                -- Y labels as one label may denote a whole list 
                -- or an array of data providers.
                --
                -- Here you can define a colour or style of
                -- the plot lines.
                xyChartBottomAxis :: LayoutAxis Double ->
                                     LayoutAxis Double,
                -- ^ A transformation of the bottom axis, 
                -- after the X title is added.
                xyChartLayout :: LayoutLR Double Double Double ->
                                 LayoutLR Double Double Double
                -- ^ A transformation of the plot layout, 
                -- where you can redefine the axes, for example.
              }
  
-- | The default time series view.  
defaultXYChartView :: XYChartView
defaultXYChartView = 
  XYChartView { xyChartTitle       = "XY Chart",
                xyChartDescription = "It shows the XY chart(s).",
                xyChartWidth       = 640,
                xyChartHeight      = 480,
                xyChartFileName    = UniqueFilePath "XYChart($RUN_INDEX)",
                xyChartPredicate   = return True,
                xyChartTransform    = id,
                xyChartXSeries      = mempty, 
                xyChartLeftYSeries  = mempty,
                xyChartRightYSeries = mempty,
                xyChartPlotTitle   = "$TITLE",
                xyChartRunPlotTitle  = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                xyChartPlotLines   = colourisePlotLines,
                xyChartBottomAxis  = id,
                xyChartLayout      = id }

instance ChartRendering r => ExperimentView XYChartView (WebPageRenderer r) where
  
  outputView v = 
    let reporter exp (WebPageRenderer renderer) dir =
          do st <- newXYChart v exp renderer dir
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = xyChartTOCHtml st,
                                   reporterWriteHtml    = xyChartHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateXYChart st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }

instance ChartRendering r => ExperimentView XYChartView (FileRenderer r) where
  
  outputView v = 
    let reporter exp (FileRenderer renderer) dir =
          do st <- newXYChart v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateXYChart st,
                                         reporterContext    = FileContext }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data XYChartViewState r =
  XYChartViewState { xyChartView       :: XYChartView,
                     xyChartExperiment :: Experiment,
                     xyChartRenderer   :: r,
                     xyChartDir        :: FilePath, 
                     xyChartMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newXYChart :: ChartRendering r => XYChartView -> Experiment -> r -> FilePath -> ExperimentWriter (XYChartViewState r)
newXYChart view exp renderer dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i ->
       resolveFilePath dir $
       mapFilePath (flip replaceExtension $ renderableChartExtension renderer) $
       expandFilePath (xyChartFileName view) $
       M.fromList [("$TITLE", xyChartTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     liftIO $ forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return XYChartViewState { xyChartView       = view,
                               xyChartExperiment = exp,
                               xyChartRenderer   = renderer,
                               xyChartDir        = dir, 
                               xyChartMap        = m }
       
-- | Plot the XY chart during the simulation.
simulateXYChart :: ChartRendering r => XYChartViewState r -> ExperimentData -> Event DisposableEvent
simulateXYChart st expdata =
  do let view    = xyChartView st
         rs0     = xyChartXSeries view $
                   xyChartTransform view $
                   experimentResults expdata
         rs1     = xyChartLeftYSeries view $
                   xyChartTransform view $
                   experimentResults expdata
         rs2     = xyChartRightYSeries view $
                   xyChartTransform view $
                   experimentResults expdata
         ext0    =
           case resultsToDoubleValues rs0 of
             [x] -> x
             _   -> error "Expected to see a single X series: simulateXYChart"
         exts1   = resultsToDoubleValues rs1
         exts2   = resultsToDoubleValues rs2
         signals = experimentPredefinedSignals expdata
         n = experimentRunCount $ xyChartExperiment st
         width   = xyChartWidth view
         height  = xyChartHeight view
         predicate  = xyChartPredicate view
         title   = xyChartTitle view
         plotTitle  = xyChartPlotTitle view
         runPlotTitle = xyChartRunPlotTitle view
         plotLines  = xyChartPlotLines view
         plotBottomAxis = xyChartBottomAxis view
         plotLayout = xyChartLayout view
         renderer   = xyChartRenderer st
     i <- liftParameter simulationIndex
     let file  = fromJust $ M.lookup (i - 1) (xyChartMap st)
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
           let x = resultValueData ext0
               y = resultValueData ext
               transform () =
                 do p <- predicate
                    if p
                      then liftM2 (,) x y
                      else return (1/0, 1/0)  -- such values will be ignored then
               signalx = resultValueSignal ext0
               signaly = resultValueSignal ext
           in newSignalHistory $
              mapSignalM transform $
              pureResultSignal signals $
              signalx <> signaly
     hs1 <- inputHistory exts1
     hs2 <- inputHistory exts2
     return $
       DisposableEvent $
       do let plots hs exts plotLineTails =
                do ps <-
                     forM (zip3 hs exts (head plotLineTails)) $
                     \(h, ext, plotLines) ->
                     do (ts, zs) <- readSignalHistory h 
                        return $
                          toPlot $
                          plotLines $
                          plot_lines_values .~ filterPlotLinesValues (elems zs) $
                          plot_lines_title .~ resultValueName ext $
                          def
                   return (ps, drop (length hs) plotLineTails)     
          (ps1, plotLineTails) <- plots hs1 exts1 (tails plotLines)
          (ps2, plotLineTails) <- plots hs2 exts2 plotLineTails
          let ps1' = map Left ps1
              ps2' = map Right ps2
              ps'  = ps1' ++ ps2'
              axis = plotBottomAxis $
                      laxis_title .~ resultValueName ext0 $
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
               when (experimentVerbose $ xyChartExperiment st) $
                 putStr "Generated file " >> putStrLn file
     
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [(Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) .
  divideBy (\(x, y) -> isNaN x || isInfinite x || isNaN y || isInfinite y)

-- | Get the HTML code.     
xyChartHtml :: XYChartViewState r -> Int -> HtmlWriter ()     
xyChartHtml st index =
  let n = experimentRunCount $ xyChartExperiment st
  in if n == 1
     then xyChartHtmlSingle st index
     else xyChartHtmlMultiple st index
     
-- | Get the HTML code for a single run.
xyChartHtmlSingle :: XYChartViewState r -> Int -> HtmlWriter ()
xyChartHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (xyChartMap st)
     writeHtmlParagraph $
       writeHtmlImage (makeRelative (xyChartDir st) f)

-- | Get the HTML code for multiple runs.
xyChartHtmlMultiple :: XYChartViewState r -> Int -> HtmlWriter ()
xyChartHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ xyChartExperiment st
     forM_ [0..(n - 1)] $ \i ->
       let f = fromJust $ M.lookup i (xyChartMap st)
       in writeHtmlParagraph $
          writeHtmlImage (makeRelative (xyChartDir st) f)

header :: XYChartViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (xyChartTitle $ xyChartView st)
     let description = xyChartDescription $ xyChartView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
xyChartTOCHtml :: XYChartViewState r -> Int -> HtmlWriter ()
xyChartTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (xyChartTitle $ xyChartView st)
