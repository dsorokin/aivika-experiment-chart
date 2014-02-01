
-- |
-- Module     : Simulation.Aivika.Experiment.DeviationChartView
-- Copyright  : Copyright (c) 2012-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'DeviationChartView' that saves the deviation
-- chart in the PNG file.
--

module Simulation.Aivika.Experiment.DeviationChartView
       (DeviationChartView(..), 
        defaultDeviationChartView) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar
import Control.Lens

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Array
import Data.Array.IO.Safe
import Data.Default.Class

import System.IO
import System.FilePath

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (divideBy, replace)
import Simulation.Aivika.Experiment.Chart (colourisePlotLines, colourisePlotFillBetween)
import Simulation.Aivika.Experiment.SamplingStatsSource

import Simulation.Aivika.Specs
import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Statistics

-- | Defines the 'View' that saves the deviation chart
-- in the PNG file.
data DeviationChartView =
  DeviationChartView { deviationChartTitle       :: String,
                       -- ^ This is a title used in HTML.
                       deviationChartDescription :: String,
                       -- ^ This is a description used in HTML.
                       deviationChartWidth       :: Int,
                       -- ^ The width of the chart.
                       deviationChartHeight      :: Int,
                       -- ^ The height of the chart.
                       deviationChartFileName    :: FileName,
                       -- ^ It defines the file name for the PNG file. 
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   deviationChartFileName = UniqueFileName \"$TITLE\" \".png\"
                       -- @
                       deviationChartSeries      :: [Either String String],
                       -- ^ It contains the labels of data plotted
                       -- on the chart.
                       deviationChartPlotTitle :: String,
                       -- ^ This is a title used in the chart. 
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   deviationChartPlotTitle = \"$TITLE\"
                       -- @
                       deviationChartPlotLines :: [PlotLines Double Double ->
                                                   PlotLines Double Double],
                       -- ^ Probably, an infinite sequence of plot 
                       -- transformations based on which the plot
                       -- is constructed for each series. Generally,
                       -- it may not coincide with a sequence of 
                       -- labels as one label may denote a whole list 
                       -- or an array of data providers.
                       --
                       -- Here you can define a colour or style of
                       -- the plot lines.
                       deviationChartPlotFillBetween :: [PlotFillBetween Double Double ->
                                                         PlotFillBetween Double Double],
                       -- ^ Corresponds exactly to 'deviationChartPlotLines'
                       -- but used for plotting the deviation areas
                       -- by the rule of 3-sigma, while the former 
                       -- is used for plotting the trends of the 
                       -- random processes.
                       deviationChartBottomAxis :: LayoutAxis Double ->
                                                   LayoutAxis Double,
                       -- ^ A transformation of the bottom axis, 
                       -- after title @time@ is added.
                       deviationChartLayout :: LayoutLR Double Double Double ->
                                               LayoutLR Double Double Double
                       -- ^ A transformation of the plot layout, 
                       -- where you can redefine the axes, for example.
                 }
  
-- | The default deviation chart view.  
defaultDeviationChartView :: DeviationChartView
defaultDeviationChartView = 
  DeviationChartView { deviationChartTitle       = "Deviation Chart",
                       deviationChartDescription = "It shows the Deviation chart by rule 3-sigma.",
                       deviationChartWidth       = 640,
                       deviationChartHeight      = 480,
                       deviationChartFileName    = UniqueFileName "$TITLE" ".png",
                       deviationChartSeries      = [], 
                       deviationChartPlotTitle   = "$TITLE",
                       deviationChartPlotLines   = colourisePlotLines,
                       deviationChartPlotFillBetween = colourisePlotFillBetween,
                       deviationChartBottomAxis  = id,
                       deviationChartLayout      = id }

instance View DeviationChartView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newDeviationChart v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = finaliseDeviationChart st,
                               reporterSimulate   = simulateDeviationChart st,
                               reporterTOCHtml    = deviationChartTOCHtml st,
                               reporterHtml       = deviationChartHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data DeviationChartViewState =
  DeviationChartViewState { deviationChartView       :: DeviationChartView,
                            deviationChartExperiment :: Experiment,
                            deviationChartDir        :: FilePath, 
                            deviationChartFile       :: IORef (Maybe FilePath),
                            deviationChartLock       :: MVar (),
                            deviationChartResults    :: IORef (Maybe DeviationChartResults) }

-- | The deviation chart item.
data DeviationChartResults =
  DeviationChartResults { deviationChartTimes :: IOArray Int Double,
                          deviationChartNames :: [Either String String],
                          deviationChartStats :: [IOArray Int (SamplingStats Double)] }
  
-- | Create a new state of the view.
newDeviationChart :: DeviationChartView -> Experiment -> FilePath -> IO DeviationChartViewState
newDeviationChart view exp dir =
  do f <- newIORef Nothing
     l <- newMVar () 
     r <- newIORef Nothing
     return DeviationChartViewState { deviationChartView       = view,
                                      deviationChartExperiment = exp,
                                      deviationChartDir        = dir, 
                                      deviationChartFile       = f,
                                      deviationChartLock       = l, 
                                      deviationChartResults    = r }
       
-- | Create new chart results.
newDeviationChartResults :: [Either String String] -> Experiment -> IO DeviationChartResults
newDeviationChartResults names exp =
  do let specs = experimentSpecs exp
         bnds  = integIterationBnds specs
     times <- liftIO $ newListArray bnds (integTimes specs)
     stats <- forM names $ \_ -> 
       liftIO $ newArray bnds emptySamplingStats
     return DeviationChartResults { deviationChartTimes = times,
                                    deviationChartNames = names,
                                    deviationChartStats = stats }
       
-- | Simulate the specified series.
simulateDeviationChart :: DeviationChartViewState -> ExperimentData -> Event (Event ())
simulateDeviationChart st expdata =
  do let labels = deviationChartSeries $ deviationChartView st
         (leftLabels, rightLabels) = partitionEithers labels 
         (leftProviders, rightProviders) =
           (experimentSeriesProviders expdata leftLabels,
            experimentSeriesProviders expdata rightLabels)
         providerInput providers =
           flip map providers $ \provider ->
           case providerToDoubleStatsSource provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as a source of double values: simulateDeviationChart"
             Just input -> (providerName provider,
                            provider,
                            samplingStatsSourceData input)
         leftInput = providerInput leftProviders
         rightInput = providerInput rightProviders
         leftNames = flip map leftInput $ \(x, _, _) -> Left x
         rightNames = flip map rightInput $ \(x, _, _) -> Right x
         input = leftInput ++ rightInput
         names = leftNames ++ rightNames
         source = flip map input $ \(_, _, x) -> x 
         exp = deviationChartExperiment st
         lock = deviationChartLock st
     results <- liftIO $ readIORef (deviationChartResults st)
     case results of
       Nothing ->
         liftIO $
         do results <- newDeviationChartResults names exp
            writeIORef (deviationChartResults st) $ Just results
       Just results ->
         when (names /= deviationChartNames results) $
         error "Series with different names are returned for different runs: simulateDeviationChart"
     results <- liftIO $ fmap fromJust $ readIORef (deviationChartResults st)
     let stats = deviationChartStats results
         h = experimentSignalInIntegTimes expdata
     handleSignal_ h $ \_ ->
       do xs <- sequence source
          i  <- liftDynamics integIteration
          liftIO $ withMVar lock $ \() ->
            forM_ (zip xs stats) $ \(x, stats) ->
            do y <- readArray stats i
               let y' = addDataToSamplingStats x y
               y' `seq` writeArray stats i y'
     return $ return ()
     
-- | Plot the deviation chart after the simulation is complete.
finaliseDeviationChart :: DeviationChartViewState -> IO ()
finaliseDeviationChart st =
  do let title = deviationChartTitle $ deviationChartView st
         plotTitle = 
           replace "$TITLE" title
           (deviationChartPlotTitle $ deviationChartView st)
         width = deviationChartWidth $ deviationChartView st
         height = deviationChartHeight $ deviationChartView st
         plotLines = deviationChartPlotLines $ deviationChartView st
         plotFillBetween = deviationChartPlotFillBetween $ deviationChartView st
         plotBottomAxis = deviationChartBottomAxis $ deviationChartView st
         plotLayout = deviationChartLayout $ deviationChartView st
     results <- readIORef $ deviationChartResults st
     case results of
       Nothing -> return ()
       Just results ->
         do let times = deviationChartTimes results
                names = deviationChartNames results
                stats = deviationChartStats results
            ps1 <- forM (zip3 names stats plotLines) $ \(name, stats, plotLines) ->
              do xs <- getAssocs stats
                 zs <- forM xs $ \(i, stats) ->
                   do t <- readArray times i
                      return (t, samplingStatsMean stats)
                 let p = toPlot $
                         plotLines $
                         plot_lines_values .~ filterPlotLinesValues zs $
                         plot_lines_title .~ either id id name $
                         def
                 case name of
                   Left _  -> return $ Left p
                   Right _ -> return $ Right p
            ps2 <- forM (zip3 names stats plotFillBetween) $ \(name, stats, plotFillBetween) ->
              do xs <- getAssocs stats
                 zs <- forM xs $ \(i, stats) ->
                   do t <- readArray times i
                      let mu    = samplingStatsMean stats
                          sigma = samplingStatsDeviation stats
                      return (t, (mu - 3 * sigma, mu + 3 * sigma))
                 let p = toPlot $
                         plotFillBetween $
                         plot_fillbetween_values .~ filterPlotFillBetweenValues zs $
                         plot_fillbetween_title .~ either id id name $
                         def
                 case name of
                   Left _  -> return $ Left p
                   Right _ -> return $ Right p
            let ps = join $ flip map (zip ps1 ps2) $ \(p1, p2) -> [p2, p1]
                axis = plotBottomAxis $
                       laxis_title .~ "time" $
                       def
                updateLeftAxis =
                  if null $ lefts ps
                  then layoutlr_left_axis_visibility .~ AxisVisibility False False False
                  else id
                updateRightAxis =
                  if null $ rights ps
                  then layoutlr_right_axis_visibility .~ AxisVisibility False False False
                  else id
                chart = plotLayout . updateLeftAxis . updateRightAxis $
                        layoutlr_x_axis .~ axis $
                        layoutlr_title .~ plotTitle $
                        layoutlr_plots .~ ps $
                        def
            file <- resolveFileName 
                    (Just $ deviationChartDir st)
                    (deviationChartFileName $ deviationChartView st) $
                    M.fromList [("$TITLE", title)]
            let opts = FileOptions (width, height) PNG
            renderableToFile opts (toRenderable chart) file
            when (experimentVerbose $ deviationChartExperiment st) $
              putStr "Generated file " >> putStrLn file
            writeIORef (deviationChartFile st) $ Just file
     
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [(Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) .
  divideBy (\(t, x) -> isNaN x || isInfinite x)

-- | Remove the NaN and inifity values.     
filterPlotFillBetweenValues :: [(Double, (Double, Double))] -> [(Double, (Double, Double))]
filterPlotFillBetweenValues = 
  filter $ \(t, (x1, x2)) -> not $ isNaN x1 || isInfinite x1 || isNaN x2 || isInfinite x2

-- | Get the HTML code.     
deviationChartHtml :: DeviationChartViewState -> Int -> HtmlWriter ()
deviationChartHtml st index =
  do header st index
     file <- liftIO $ readIORef (deviationChartFile st)
     case file of
       Nothing -> return ()
       Just f  ->
         writeHtmlParagraph $
         writeHtmlImage (makeRelative (deviationChartDir st) f)

header :: DeviationChartViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (deviationChartTitle $ deviationChartView st)
     let description = deviationChartDescription $ deviationChartView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
deviationChartTOCHtml :: DeviationChartViewState -> Int -> HtmlWriter ()
deviationChartTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (deviationChartTitle $ deviationChartView st)
