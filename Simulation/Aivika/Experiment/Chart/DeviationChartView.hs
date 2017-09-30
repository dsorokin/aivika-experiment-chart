
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.DeviationChartView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'DeviationChartView' that plots the deviation chart using rule of 3-sigma.
--

module Simulation.Aivika.Experiment.Chart.DeviationChartView
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
import Data.Monoid
import Data.Array
import Data.Array.IO.Safe
import Data.Default.Class

import System.IO
import System.FilePath

import Graphics.Rendering.Chart

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Base
import Simulation.Aivika.Experiment.Concurrent.MVar
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines, colourisePlotFillBetween)

-- | Defines the 'View' that plots the deviation chart in time points by the specified grid.
data DeviationChartView =
  DeviationChartView { deviationChartTitle       :: String,
                       -- ^ This is a title used in HTML.
                       deviationChartDescription :: String,
                       -- ^ This is a description used in HTML.
                       deviationChartWidth       :: Int,
                       -- ^ The width of the chart.
                       deviationChartHeight      :: Int,
                       -- ^ The height of the chart.
                       deviationChartGridSize    :: Int,
                       -- ^ The size of the grid, where the series data are collected.
                       deviationChartFileName    :: ExperimentFilePath,
                       -- ^ It defines the file name with optional extension for each image to be saved.
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   deviationChartFileName = UniqueFilePath \"$TITLE\"
                       -- @
                       deviationChartTransform :: ResultTransform,
                       -- ^ The transform applied to the results before receiving series.
                       deviationChartLeftYSeries  :: ResultTransform, 
                       -- ^ It defines the series to be plotted basing on the left Y axis.
                       deviationChartRightYSeries :: ResultTransform, 
                       -- ^ It defines the series to be plotted basing on the right Y axis.
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
                       deviationChartGridSize    = 2 * 640,
                       deviationChartFileName    = UniqueFilePath "DeviationChart",
                       deviationChartTransform   = id,
                       deviationChartLeftYSeries  = mempty, 
                       deviationChartRightYSeries = mempty, 
                       deviationChartPlotTitle   = "$TITLE",
                       deviationChartPlotLines   = colourisePlotLines,
                       deviationChartPlotFillBetween = colourisePlotFillBetween,
                       deviationChartBottomAxis  = id,
                       deviationChartLayout      = id }

instance ChartRendering r => ExperimentView DeviationChartView (WebPageRenderer r) where
  
  outputView v = 
    let reporter exp (WebPageRenderer renderer _) dir =
          do st <- newDeviationChart v exp renderer dir
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = deviationChartTOCHtml st,
                                   reporterWriteHtml    = deviationChartHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseDeviationChart st,
                                         reporterSimulate   = simulateDeviationChart st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }

instance ChartRendering r => ExperimentView DeviationChartView (FileRenderer r) where
  
  outputView v = 
    let reporter exp (FileRenderer renderer _) dir =
          do st <- newDeviationChart v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseDeviationChart st,
                                         reporterSimulate   = simulateDeviationChart st,
                                         reporterContext    = FileContext }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data DeviationChartViewState r =
  DeviationChartViewState { deviationChartView       :: DeviationChartView,
                            deviationChartExperiment :: Experiment,
                            deviationChartRenderer   :: r,
                            deviationChartDir        :: FilePath, 
                            deviationChartFile       :: IORef (Maybe FilePath),
                            deviationChartResults    :: MVar (Maybe DeviationChartResults) }

-- | The deviation chart item.
data DeviationChartResults =
  DeviationChartResults { deviationChartTimes :: IOArray Int Double,
                          deviationChartNames :: [Either String String],
                          deviationChartStats :: [MVar (IOArray Int (SamplingStats Double))] }
  
-- | Create a new state of the view.
newDeviationChart :: DeviationChartView -> Experiment -> r -> FilePath -> ExperimentWriter (DeviationChartViewState r)
newDeviationChart view exp renderer dir =
  liftIO $ 
  do f <- newIORef Nothing
     r <- newMVar Nothing
     return DeviationChartViewState { deviationChartView       = view,
                                      deviationChartExperiment = exp,
                                      deviationChartRenderer   = renderer,
                                      deviationChartDir        = dir, 
                                      deviationChartFile       = f,
                                      deviationChartResults    = r }
       
-- | Create new chart results.
newDeviationChartResults :: DeviationChartViewState r -> [Either String String] -> IO DeviationChartResults
newDeviationChartResults st names =
  do let exp   = deviationChartExperiment st
         view  = deviationChartView st
         size  = deviationChartGridSize view
         specs = experimentSpecs exp
         grid  = timeGrid specs size
         bnds  = (0, 1 + length grid)
     times <- liftIO $ newListArray bnds $ map snd grid
     stats <- forM names $ \_ -> 
       liftIO $ newArray bnds emptySamplingStats >>= newMVar
     return DeviationChartResults { deviationChartTimes = times,
                                    deviationChartNames = names,
                                    deviationChartStats = stats }

-- | Require to return unique chart results associated with the specified state. 
requireDeviationChartResults :: DeviationChartViewState r -> [Either String String] -> IO DeviationChartResults
requireDeviationChartResults st names =
  maybePutMVar (deviationChartResults st)
  (newDeviationChartResults st names) $ \results ->
  if (names /= deviationChartNames results)
  then error "Series with different names are returned for different runs: requireDeviationChartResults"
  else return results
       
-- | Simulate the specified series.
simulateDeviationChart :: DeviationChartViewState r -> ExperimentData -> Composite ()
simulateDeviationChart st expdata =
  do let view    = deviationChartView st
         loc     = localisePathResultTitle $
                   experimentLocalisation $
                   deviationChartExperiment st
         rs1     = deviationChartLeftYSeries view $
                   deviationChartTransform view $
                   experimentResults expdata
         rs2     = deviationChartRightYSeries view $
                   deviationChartTransform view $
                   experimentResults expdata
         exts1   = resultsToDoubleStatsEitherValues rs1
         exts2   = resultsToDoubleStatsEitherValues rs2
         exts    = exts1 ++ exts2
         names1  = map (loc . resultValueIdPath) exts1
         names2  = map (loc . resultValueIdPath) exts2
         names   = map Left names1 ++ map Right names2
     signal <- liftEvent $
               newSignalInTimeGrid $
               deviationChartGridSize view
     hs <- forM exts $ \ext ->
           newSignalHistory $
           flip mapSignalM signal $ \i ->
           resultValueData ext
     disposableComposite $
       DisposableEvent $
       do results <- liftIO $ requireDeviationChartResults st names
          let stats = deviationChartStats results
          forM_ (zip hs stats) $ \(h, stats') ->
            do (ts, xs) <- readSignalHistory h
               let (lo, hi) = bounds ts
               liftIO $
                 withMVar stats' $ \stats'' ->
                 forM_ [lo..hi] $ \i ->
                 do let x = xs ! i
                    y <- readArray stats'' i
                    let y' = combineSamplingStatsEither x y
                    y' `seq` writeArray stats'' i y'
     
-- | Plot the deviation chart after the simulation is complete.
finaliseDeviationChart :: ChartRendering r => DeviationChartViewState r -> ExperimentWriter ()
finaliseDeviationChart st =
  do let view = deviationChartView st
         title = deviationChartTitle view
         plotTitle = deviationChartPlotTitle view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         width = deviationChartWidth view
         height = deviationChartHeight view
         plotLines = deviationChartPlotLines view
         plotFillBetween = deviationChartPlotFillBetween view
         plotBottomAxis = deviationChartBottomAxis view
         plotLayout = deviationChartLayout view
         renderer = deviationChartRenderer st
     file <- resolveFilePath (deviationChartDir st) $
             mapFilePath (flip replaceExtension $ renderableChartExtension renderer) $
             expandFilePath (deviationChartFileName view) $
             M.fromList [("$TITLE", title)]
     results <- liftIO $ readMVar $ deviationChartResults st
     case results of
       Nothing -> return ()
       Just results ->
         liftIO $
         do let times = deviationChartTimes results
                names = deviationChartNames results
                stats = deviationChartStats results
            ps1 <- forM (zip3 names stats plotLines) $ \(name, stats', plotLines) ->
              do stats'' <- readMVar stats'
                 xs <- getAssocs stats''
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
            ps2 <- forM (zip3 names stats plotFillBetween) $ \(name, stats', plotFillBetween) ->
              do stats'' <- readMVar stats'
                 xs <- getAssocs stats''
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
                chart = plotLayout .
                        renderingLayoutLR renderer .
                        updateLeftAxis . updateRightAxis $
                        layoutlr_x_axis .~ axis $
                        layoutlr_title .~ plotTitle' $
                        layoutlr_plots .~ ps $
                        def
            renderChart renderer (width, height) file (toRenderable chart)
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
deviationChartHtml :: DeviationChartViewState r -> Int -> HtmlWriter ()
deviationChartHtml st index =
  do header st index
     file <- liftIO $ readIORef (deviationChartFile st)
     case file of
       Nothing -> return ()
       Just f  ->
         writeHtmlParagraph $
         writeHtmlImage (makeRelative (deviationChartDir st) f)

header :: DeviationChartViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (deviationChartTitle $ deviationChartView st)
     let description = deviationChartDescription $ deviationChartView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
deviationChartTOCHtml :: DeviationChartViewState r -> Int -> HtmlWriter ()
deviationChartTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (deviationChartTitle $ deviationChartView st)
