
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.XYChartView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'XYChartView' that saves the XY charts.
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

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (divideBy, replace)
import Simulation.Aivika.Experiment.Chart.ChartRenderer
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines)

import Simulation.Aivika.Specs
import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Signal

-- | Defines the 'View' that saves the XY charts.
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
                xyChartXSeries     :: Maybe String,
                -- ^ This is a label of the X series.
                --
                -- You must define it, because it is 'Nothing' 
                -- by default.
                xyChartYSeries      :: [Either String String],
                -- ^ It contains the labels of Y series plotted
                -- on the XY chart.
                xyChartPlotTitle :: String,
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
                xyChartFileName    = UniqueFilePath "$TITLE - $RUN_INDEX",
                xyChartPredicate   = return True,
                xyChartXSeries     = Nothing, 
                xyChartYSeries     = [],
                xyChartPlotTitle   = "$TITLE",
                xyChartRunPlotTitle  = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                xyChartPlotLines   = colourisePlotLines,
                xyChartBottomAxis  = id,
                xyChartLayout      = id }

instance ChartRenderer r => ExperimentView XYChartView r where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newXYChart v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateXYChart st,
                                         reporterTOCHtml    = xyChartTOCHtml st,
                                         reporterHtml       = xyChartHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data XYChartViewState r =
  XYChartViewState { xyChartView       :: XYChartView,
                     xyChartExperiment :: Experiment r,
                     xyChartRenderer   :: r,
                     xyChartDir        :: FilePath, 
                     xyChartMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newXYChart :: ChartRenderer r =>
              XYChartView -> Experiment r -> r -> FilePath -> IO (XYChartViewState r)
newXYChart view exp renderer dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i ->
       resolveFilePath dir $
       mapFilePath (flip replaceExtension $ renderableFileExtension renderer) $
       expandFilePath (xyChartFileName view) $
       M.fromList [("$TITLE", xyChartTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return XYChartViewState { xyChartView       = view,
                               xyChartExperiment = exp,
                               xyChartRenderer   = renderer,
                               xyChartDir        = dir, 
                               xyChartMap        = m }
       
-- | Plot the XY chart during the simulation.
simulateXYChart :: ChartRenderer r => XYChartViewState r -> ExperimentData -> Event (Event ())
simulateXYChart st expdata =
  do let ylabels = xyChartYSeries $ xyChartView st
         xlabels = xyChartXSeries $ xyChartView st
         xlabel  = flip fromMaybe xlabels $
                   error "X series is not provided: simulateXYChart"
         (leftYLabels, rightYLabels) = partitionEithers ylabels
         leftYProviders  = experimentSeriesProviders expdata leftYLabels
         rightYProviders = experimentSeriesProviders expdata rightYLabels
         xprovider  = 
           case experimentSeriesProviders expdata [xlabel] of
             [provider] -> provider
             _ -> error $
                  "Only a single X series must be" ++
                  " provided: simulateXYChart"
         providerInput providers =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateXYChart"
             Just input -> (providerName provider, provider, input)
         leftYInput  = providerInput leftYProviders
         rightYInput = providerInput rightYProviders
         [(xname, _, x)] = providerInput [xprovider]
         n = experimentRunCount $ xyChartExperiment st
         width = xyChartWidth $ xyChartView st
         height = xyChartHeight $ xyChartView st
         predicate = xyChartPredicate $ xyChartView st
         plotLines = xyChartPlotLines $ xyChartView st
         plotBottomAxis = xyChartBottomAxis $ xyChartView st
         plotLayout = xyChartLayout $ xyChartView st
         renderer = xyChartRenderer st
     i <- liftParameter simulationIndex
     let file = fromJust $ M.lookup (i - 1) (xyChartMap st)
         title = xyChartTitle $ xyChartView st
         plotTitle = 
           replace "$TITLE" title
           (xyChartPlotTitle $ xyChartView st)
         runPlotTitle =
           if n == 1
           then plotTitle
           else replace "$RUN_INDEX" (show i) $
                replace "$RUN_COUNT" (show n) $
                replace "$PLOT_TITLE" plotTitle
                (xyChartRunPlotTitle $ xyChartView st)
         inputHistory input = 
           forM input $ \(name, provider, y) ->
           let transform () =
                 do p <- predicate
                    if p
                      then liftM2 (,) x y
                      else return (1/0, 1/0)  -- such values will be ignored then
           in newSignalHistory $
              mapSignalM transform $
              experimentMixedSignal expdata [provider] <>
              experimentMixedSignal expdata [xprovider]
     leftHs  <- inputHistory leftYInput
     rightHs <- inputHistory rightYInput
     return $
       do let plots hs input plotLineTails =
                do ps <-
                     forM (zip3 hs input (head plotLineTails)) $
                     \(h, (name, provider, input), plotLines) ->
                     do (ts, zs) <- readSignalHistory h 
                        return $
                          toPlot $
                          plotLines $
                          plot_lines_values .~ filterPlotLinesValues (elems zs) $
                          plot_lines_title .~ name $
                          def
                   return (ps, drop (length hs) plotLineTails)     
          (leftPs, plotLineTails) <- plots leftHs leftYInput (tails plotLines)
          (rightPs, plotLineTails) <- plots rightHs rightYInput plotLineTails
          let leftPs' = map Left leftPs
              rightPs' = map Right rightPs
              ps' = leftPs' ++ rightPs'
              axis  = plotBottomAxis $
                      laxis_title .~ providerName xprovider $
                      def
              updateLeftAxis =
                if null leftPs
                then layoutlr_left_axis_visibility .~ AxisVisibility False False False
                else id
              updateRightAxis =
                if null rightPs
                then layoutlr_right_axis_visibility .~ AxisVisibility False False False
                else id
              chart = plotLayout . updateLeftAxis . updateRightAxis $
                      layoutlr_x_axis .~ axis $
                      layoutlr_title .~ runPlotTitle $
                      layoutlr_plots .~ ps' $
                      def
          liftIO $
            do renderChart renderer (width, height) (toRenderable chart) file
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