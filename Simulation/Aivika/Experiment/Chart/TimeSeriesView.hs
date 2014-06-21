
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.TimeSeriesView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'TimeSeriesView' that saves the time series charts.
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

-- | Defines the 'View' that saves the time series charts.
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
                   timeSeriesFileName    = UniqueFilePath "$TITLE - $RUN_INDEX",
                   timeSeriesPredicate   = return True,
                   timeSeries            = [], 
                   timeSeriesPlotTitle   = "$TITLE",
                   timeSeriesRunPlotTitle = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                   timeSeriesPlotLines   = colourisePlotLines,
                   timeSeriesBottomAxis  = id,
                   timeSeriesLayout      = id }

instance ChartRenderer r => ExperimentView TimeSeriesView r where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newTimeSeries v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateTimeSeries st,
                                         reporterTOCHtml    = timeSeriesTOCHtml st,
                                         reporterHtml       = timeSeriesHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data TimeSeriesViewState r =
  TimeSeriesViewState { timeSeriesView       :: TimeSeriesView,
                        timeSeriesExperiment :: Experiment r,
                        timeSeriesRenderer   :: r,
                        timeSeriesDir        :: FilePath, 
                        timeSeriesMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newTimeSeries :: ChartRenderer r =>
                 TimeSeriesView -> Experiment r -> r -> FilePath -> IO (TimeSeriesViewState r)
newTimeSeries view exp renderer dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i ->
       fmap (flip replaceExtension $ renderableFileExtension renderer) $
       resolveFilePath dir $
       expandFilePath (timeSeriesFileName view) $
       M.fromList [("$TITLE", timeSeriesTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return TimeSeriesViewState { timeSeriesView       = view,
                                  timeSeriesExperiment = exp,
                                  timeSeriesRenderer   = renderer,
                                  timeSeriesDir        = dir, 
                                  timeSeriesMap        = m }
       
-- | Plot the time series chart during the simulation.
simulateTimeSeries :: ChartRenderer r => TimeSeriesViewState r -> ExperimentData -> Event (Event ())
simulateTimeSeries st expdata =
  do let labels = timeSeries $ timeSeriesView st
         (leftLabels, rightLabels) = partitionEithers labels 
         (leftProviders, rightProviders) =
           (experimentSeriesProviders expdata leftLabels,
            experimentSeriesProviders expdata rightLabels)
         providerInput providers =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateTimeSeries"
             Just input -> (providerName provider, provider, input)
         leftInput = providerInput leftProviders
         rightInput = providerInput rightProviders
         n = experimentRunCount $ timeSeriesExperiment st
         width = timeSeriesWidth $ timeSeriesView st
         height = timeSeriesHeight $ timeSeriesView st
         predicate = timeSeriesPredicate $ timeSeriesView st
         plotLines = timeSeriesPlotLines $ timeSeriesView st
         plotBottomAxis = timeSeriesBottomAxis $ timeSeriesView st
         plotLayout = timeSeriesLayout $ timeSeriesView st
         renderer = timeSeriesRenderer st
     i <- liftParameter simulationIndex
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
         inputHistory input =
           forM input $ \(name, provider, input) ->
           let transform () =
                 do x <- predicate
                    if x then input else return (1/0)  -- the infinite values will be ignored then
           in newSignalHistory $
              mapSignalM transform $
              experimentMixedSignal expdata [provider]
     leftHs <- inputHistory leftInput
     rightHs <- inputHistory rightInput
     return $
       do let plots hs input plotLineTails =
                do ps <-
                     forM (zip3 hs input (head plotLineTails)) $
                     \(h, (name, provider, input), plotLines) ->
                     do (ts, xs) <- readSignalHistory h 
                        return $
                          toPlot $
                          plotLines $
                          plot_lines_values .~ filterPlotLinesValues (zip (elems ts) (elems xs)) $
                          plot_lines_title .~ name $
                          def
                   return (ps, drop (length hs) plotLineTails)
          (leftPs, plotLineTails) <- plots leftHs leftInput (tails plotLines)
          (rightPs, plotLineTails) <- plots rightHs rightInput plotLineTails
          let leftPs' = map Left leftPs
              rightPs' = map Right rightPs
              ps' = leftPs' ++ rightPs'
              axis  = plotBottomAxis $
                      laxis_title .~ "time" $
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
