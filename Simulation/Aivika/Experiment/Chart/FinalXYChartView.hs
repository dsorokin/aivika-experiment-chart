
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.FinalXYChartView
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines 'FinalXYChartView' that plots a single XY chart
-- in final time points for different simulation runs sequentially
-- by the run index.
--

module Simulation.Aivika.Experiment.Chart.FinalXYChartView
       (FinalXYChartView(..), 
        defaultFinalXYChartView) where

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
import Simulation.Aivika.Experiment.MRef
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines)

-- | Defines the 'View' that plots the XY chart
-- in final time points for different simulation runs
-- sequentially by the run index.
data FinalXYChartView =
  FinalXYChartView { finalXYChartTitle       :: String,
                     -- ^ This is a title used HTML.
                     finalXYChartDescription :: String,
                     -- ^ This is a description used in HTML.
                     finalXYChartWidth       :: Int,
                     -- ^ The width of the chart.
                     finalXYChartHeight      :: Int,
                     -- ^ The height of the chart.
                     finalXYChartFileName    :: ExperimentFilePath,
                     -- ^ It defines the file name with optional extension for each image to be saved.
                     -- It may include special variable @$TITLE@.
                     --
                     -- An example is
                     --
                     -- @
                     --   finalXYChartFileName = UniqueFilePath \"$TITLE\"
                     -- @
                     finalXYChartPredicate   :: Event Bool,
                     -- ^ It specifies the predicate that defines
                     -- when we count data when plotting the chart.
                     finalXYChartTransform   :: ResultTransform,
                     -- ^ The transform applied to the results before receiving series.
                     finalXYChartXSeries     :: ResultTransform,
                     -- ^ This is the X series.
                     --
                     -- You must define it, because it is 'mempty' 
                     -- by default. Also it must return exactly
                     -- one 'ResultExtract' item when calling
                     -- function 'extractDoubleResults' by the specified
                     -- result set.
                     finalXYChartLeftYSeries  :: ResultTransform, 
                     -- ^ It defines the series to be plotted basing on the left Y axis.
                     finalXYChartRightYSeries :: ResultTransform, 
                     -- ^ It defines the series to be plotted basing on the right Y axis.
                     finalXYChartPlotTitle   :: String,
                     -- ^ This is a title used in the chart. 
                     -- It may include special variable @$TITLE@.
                     --
                     -- An example is
                     --
                     -- @
                     --   finalXYChartPlotTitle = \"$TITLE\"
                     -- @
                     finalXYChartPlotLines :: [PlotLines Double Double ->
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
                     finalXYChartBottomAxis :: LayoutAxis Double ->
                                               LayoutAxis Double,
                     -- ^ A transformation of the bottom axis, 
                     -- after the X title is added.
                     finalXYChartLayout :: LayoutLR Double Double Double ->
                                           LayoutLR Double Double Double
                     -- ^ A transformation of the plot layout, 
                     -- where you can redefine the axes, for example.
                   }
  
-- | The default XY chart view.  
defaultFinalXYChartView :: FinalXYChartView
defaultFinalXYChartView = 
  FinalXYChartView { finalXYChartTitle       = "Final XY Chart",
                     finalXYChartDescription = "It shows the XY chart for the results in the final time points.",
                     finalXYChartWidth       = 640,
                     finalXYChartHeight      = 480,
                     finalXYChartFileName    = UniqueFilePath "FinalXYChart",
                     finalXYChartPredicate   = return True,
                     finalXYChartTransform   = id,
                     finalXYChartXSeries     = mempty,
                     finalXYChartLeftYSeries  = mempty,
                     finalXYChartRightYSeries = mempty,
                     finalXYChartPlotTitle   = "$TITLE",
                     finalXYChartPlotLines   = colourisePlotLines,
                     finalXYChartBottomAxis  = id,
                     finalXYChartLayout      = id }

instance ChartRendering r => ExperimentView FinalXYChartView (WebPageRenderer r) where
  
  outputView v = 
    let reporter exp (WebPageRenderer renderer) dir =
          do st <- newFinalXYChart v exp renderer dir
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = finalXYChartTOCHtml st,
                                   reporterWriteHtml    = finalXYChartHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalXYChart st,
                                         reporterSimulate   = simulateFinalXYChart st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }

instance ChartRendering r => ExperimentView FinalXYChartView (FileRenderer r) where
  
  outputView v = 
    let reporter exp (FileRenderer renderer) dir =
          do st <- newFinalXYChart v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalXYChart st,
                                         reporterSimulate   = simulateFinalXYChart st,
                                         reporterContext    = FileContext }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalXYChartViewState r =
  FinalXYChartViewState { finalXYChartView       :: FinalXYChartView,
                          finalXYChartExperiment :: Experiment,
                          finalXYChartRenderer   :: r,
                          finalXYChartDir        :: FilePath, 
                          finalXYChartFile       :: IORef (Maybe FilePath),
                          finalXYChartLock       :: MVar (),
                          finalXYChartResults    :: MRef (Maybe FinalXYChartResults) }

-- | The XY chart results.
data FinalXYChartResults =
  FinalXYChartResults { finalXYChartXName  :: String,
                        finalXYChartYNames :: [Either String String],
                        finalXYChartXY     :: [IOArray Int (Maybe (Double, Double))] }
  
-- | Create a new state of the view.
newFinalXYChart :: FinalXYChartView -> Experiment -> r -> FilePath -> ExperimentWriter (FinalXYChartViewState r)
newFinalXYChart view exp renderer dir =
  liftIO $
  do f <- newIORef Nothing
     l <- newMVar () 
     r <- newMRef Nothing
     return FinalXYChartViewState { finalXYChartView       = view,
                                    finalXYChartExperiment = exp,
                                    finalXYChartRenderer   = renderer,
                                    finalXYChartDir        = dir, 
                                    finalXYChartFile       = f,
                                    finalXYChartLock       = l, 
                                    finalXYChartResults    = r }
       
-- | Create new chart results.
newFinalXYChartResults :: String -> [Either String String] -> Experiment -> IO FinalXYChartResults
newFinalXYChartResults xname ynames exp =
  do let n = experimentRunCount exp
     xy <- forM ynames $ \_ -> 
       liftIO $ newArray (1, n) Nothing
     return FinalXYChartResults { finalXYChartXName  = xname,
                                  finalXYChartYNames = ynames,
                                  finalXYChartXY     = xy }
       
-- | Require to return unique results associated with the specified state. 
requireFinalXYChartResults :: FinalXYChartViewState r -> String -> [Either String String] -> IO FinalXYChartResults
requireFinalXYChartResults st xname ynames =
  maybeWriteMRef (finalXYChartResults st)
  (newFinalXYChartResults xname ynames (finalXYChartExperiment st)) $ \results ->
  if (xname /= finalXYChartXName results) || (ynames /= finalXYChartYNames results)
  then error "Series with different names are returned for different runs: requireFinalXYChartResults"
  else return results
       
-- | Simulation.
simulateFinalXYChart :: FinalXYChartViewState r -> ExperimentData -> Event DisposableEvent
simulateFinalXYChart st expdata =
  do let view    = finalXYChartView st
         rs0     = finalXYChartXSeries view $
                   finalXYChartTransform view $
                   experimentResults expdata
         rs1     = finalXYChartLeftYSeries view $
                   finalXYChartTransform view $
                   experimentResults expdata
         rs2     = finalXYChartRightYSeries view $
                   finalXYChartTransform view $
                   experimentResults expdata
         ext0    =
           case resultsToDoubleValues rs0 of
             [x] -> x
             _   -> error "Expected to see a single X series: simulateFinalXYChart"
         exts1   = resultsToDoubleValues rs1
         exts2   = resultsToDoubleValues rs2
         exts    = exts1 ++ exts2
         name0   = resultValueName ext0
         names1  = map resultValueName exts1
         names2  = map resultValueName exts2
         names   = map Left names1 ++ map Right names2
         signals = experimentPredefinedSignals expdata
         signal  = filterSignalM (const predicate) $
                   resultSignalInStopTime signals
         n = experimentRunCount $ finalXYChartExperiment st
         predicate  = finalXYChartPredicate view
         lock = finalXYChartLock st
     results <- liftIO $ requireFinalXYChartResults st name0 names
     let xys = finalXYChartXY results
     handleSignal signal $ \_ ->
       do x  <- resultValueData ext0
          ys <- forM exts resultValueData
          i  <- liftParameter simulationIndex
          liftIO $
            forM_ (zip ys xys) $ \(y, xy) ->
            withMVar lock $ \() ->
            x `seq` y `seq` writeArray xy i $ Just (x, y)
     
-- | Plot the XY chart after the simulation is complete.
finaliseFinalXYChart :: ChartRendering r => FinalXYChartViewState r -> ExperimentWriter ()
finaliseFinalXYChart st =
  do let view = finalXYChartView st 
         title = finalXYChartTitle view
         plotTitle = finalXYChartPlotTitle view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         width = finalXYChartWidth view
         height = finalXYChartHeight view
         plotLines = finalXYChartPlotLines view
         plotBottomAxis = finalXYChartBottomAxis view
         plotLayout = finalXYChartLayout view
         renderer = finalXYChartRenderer st
     file <- resolveFilePath (finalXYChartDir st) $
             mapFilePath (flip replaceExtension $ renderableChartExtension renderer) $
             expandFilePath (finalXYChartFileName view) $
             M.fromList [("$TITLE", title)]
     results <- liftIO $ readMRef $ finalXYChartResults st
     case results of
       Nothing -> return ()
       Just results ->
         liftIO $
         do let xname  = finalXYChartXName results
                ynames = finalXYChartYNames results
                xys    = finalXYChartXY results
            ps <- forM (zip3 ynames xys plotLines) $ \(name, xy, plotLines) ->
              do zs <- getElems xy
                 let p = toPlot $
                         plotLines $
                         plot_lines_values .~ filterPlotLinesValues zs $
                         plot_lines_title .~ either id id name $
                         def
                     r = case name of
                       Left _  -> Left p
                       Right _ -> Right p
                 return r
            let axis = plotBottomAxis $
                       laxis_title .~ xname $
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
                        layoutlr_title .~ plotTitle' $
                        layoutlr_plots .~ ps $
                        def
            renderChart renderer (width, height) file (toRenderable chart)
            when (experimentVerbose $ finalXYChartExperiment st) $
              putStr "Generated file " >> putStrLn file
            writeIORef (finalXYChartFile st) $ Just file
     
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [Maybe (Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) . map (map fromJust) . divideBy pred
    where pred Nothing       = True
          pred (Just (x, y)) = isNaN x || isInfinite x || 
                               isNaN y || isInfinite y

-- | Get the HTML code.     
finalXYChartHtml :: FinalXYChartViewState r -> Int -> HtmlWriter ()
finalXYChartHtml st index =
  do header st index
     file <- liftIO $ readIORef (finalXYChartFile st)
     case file of
       Nothing -> return ()
       Just f  ->
         writeHtmlParagraph $
         writeHtmlImage (makeRelative (finalXYChartDir st) f)

header :: FinalXYChartViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalXYChartTitle $ finalXYChartView st)
     let description = finalXYChartDescription $ finalXYChartView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalXYChartTOCHtml :: FinalXYChartViewState r -> Int -> HtmlWriter ()
finalXYChartTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalXYChartTitle $ finalXYChartView st)
