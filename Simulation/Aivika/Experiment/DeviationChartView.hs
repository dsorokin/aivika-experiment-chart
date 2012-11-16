
-- |
-- Module     : Simulation.Aivika.Experiment.DeviationChartView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
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

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Array
import Data.Array.IO.Safe

import Data.Accessor

import System.IO
import System.FilePath

import Data.String.Utils (replace)

import Graphics.Rendering.Chart

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (divideBy)
import Simulation.Aivika.Experiment.Chart (colourisePlotLines, colourisePlotFillBetween)

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base (starttime, integIterationBnds, integTimes, integIteration)
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
                       --   deviationChartFileName = UniqueFileName \"$TITLE\", \".png\"
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
                       deviationChartLayout :: Layout1 Double Double ->
                                               Layout1 Double Double
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
simulateDeviationChart :: DeviationChartViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateDeviationChart st expdata =
  do let protolabels = deviationChartSeries $ deviationChartView st
         protoproviders = flip map protolabels $ \protolabel ->
           case protolabel of
             Left label  -> map Left $ experimentSeriesProviders expdata [label]
             Right label -> map Right $ experimentSeriesProviders expdata [label]
         joinedproviders = join protoproviders
         providers = flip map joinedproviders $ either id id         
         input =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateDeviationChart"
             Just input -> input
         names = flip map joinedproviders $ \protoprovider ->
           case protoprovider of
             Left provider  -> Left $ providerName provider
             Right provider -> Right $ providerName provider
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
     t0 <- starttime
     enqueue (experimentQueue expdata) t0 $
       do let h = experimentSignalInIntegTimes expdata
          -- we must subscribe through the event queue;
          -- otherwise, we will loose a signal in the start time,
          -- because the handleSignal_ function checks the event queue
          handleSignal_ h $ \_ ->
            do xs <- sequence input
               i  <- integIteration
               liftIO $ withMVar lock $ \() ->
                 forM_ (zip xs stats) $ \(x, stats) ->
                 do y <- readArray stats i
                    writeArray stats i $ addSamplingStats x y
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
                         plot_lines_values ^= filterPlotLinesValues zs $
                         plot_lines_title ^= either id id name $
                         defaultPlotLines
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
                         plot_fillbetween_values ^= filterPlotFillBetweenValues zs $
                         plot_fillbetween_title ^= either id id name $
                         defaultPlotFillBetween
                 case name of
                   Left _  -> return $ Left p
                   Right _ -> return $ Right p
            let ps = join $ flip map (zip ps1 ps2) $ \(p1, p2) -> [p2, p1]
                axis = plotBottomAxis $
                       laxis_title ^= "time" $
                       defaultLayoutAxis
                chart = plotLayout $
                        layout1_bottom_axis ^= axis $
                        layout1_title ^= plotTitle $
                        layout1_plots ^= ps $
                        defaultLayout1
            file <- resolveFileName 
                    (Just $ deviationChartDir st)
                    (deviationChartFileName $ deviationChartView st) $
                    M.fromList [("$TITLE", title)]
            renderableToPNGFile (toRenderable chart) width height file
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
