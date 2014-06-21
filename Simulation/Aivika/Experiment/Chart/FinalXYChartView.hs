
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.FinalXYChartView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'FinalXYChartView' that saves the XY chart
-- in final time points for all simulation runs sequentially.
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
import Data.Array
import Data.Array.IO.Safe
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
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Signal

-- | Defines the 'View' that saves the XY chart
-- in final time points for all simulation runs
-- sequentially.
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
                     finalXYChartXSeries     :: Maybe String,
                     -- ^ It defines a label of the single X series.
                     --
                     -- You must define it, because it is 'Nothing' 
                     -- by default.
                     finalXYChartYSeries     :: [Either String String],
                     -- ^ It contains the labels of Y series plotted
                     -- on the chart.
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
                     finalXYChartFileName    = UniqueFilePath "$TITLE",
                     finalXYChartPredicate   = return True,
                     finalXYChartXSeries     = Nothing,
                     finalXYChartYSeries     = [], 
                     finalXYChartPlotTitle   = "$TITLE",
                     finalXYChartPlotLines   = colourisePlotLines,
                     finalXYChartBottomAxis  = id,
                     finalXYChartLayout      = id }

instance ChartRenderer r => ExperimentView FinalXYChartView r where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newFinalXYChart v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalXYChart st,
                                         reporterSimulate   = simulateFinalXYChart st,
                                         reporterTOCHtml    = finalXYChartTOCHtml st,
                                         reporterHtml       = finalXYChartHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalXYChartViewState r =
  FinalXYChartViewState { finalXYChartView       :: FinalXYChartView,
                          finalXYChartExperiment :: Experiment r,
                          finalXYChartRenderer   :: r,
                          finalXYChartDir        :: FilePath, 
                          finalXYChartFile       :: IORef (Maybe FilePath),
                          finalXYChartLock       :: MVar (),
                          finalXYChartResults    :: IORef (Maybe FinalXYChartResults) }

-- | The XY chart results.
data FinalXYChartResults =
  FinalXYChartResults { finalXYChartXName  :: String,
                        finalXYChartYNames :: [Either String String],
                        finalXYChartXY     :: [IOArray Int (Maybe (Double, Double))] }
  
-- | Create a new state of the view.
newFinalXYChart :: FinalXYChartView -> Experiment r -> r -> FilePath -> IO (FinalXYChartViewState r)
newFinalXYChart view exp renderer dir =
  do f <- newIORef Nothing
     l <- newMVar () 
     r <- newIORef Nothing
     return FinalXYChartViewState { finalXYChartView       = view,
                                    finalXYChartExperiment = exp,
                                    finalXYChartRenderer   = renderer,
                                    finalXYChartDir        = dir, 
                                    finalXYChartFile       = f,
                                    finalXYChartLock       = l, 
                                    finalXYChartResults    = r }
       
-- | Create new chart results.
newFinalXYChartResults :: String -> [Either String String] -> Experiment r -> IO FinalXYChartResults
newFinalXYChartResults xname ynames exp =
  do let n = experimentRunCount exp
     xy <- forM ynames $ \_ -> 
       liftIO $ newArray (1, n) Nothing
     return FinalXYChartResults { finalXYChartXName  = xname,
                                  finalXYChartYNames = ynames,
                                  finalXYChartXY     = xy }
       
-- | Simulation.
simulateFinalXYChart :: FinalXYChartViewState r -> ExperimentData -> Event (Event ())
simulateFinalXYChart st expdata =
  do let ylabels = finalXYChartYSeries $ finalXYChartView st
         xlabels = finalXYChartXSeries $ finalXYChartView st
         xlabel  = flip fromMaybe xlabels $
                   error "X series is not provided: simulateFinalXYChart"
         (leftYLabels, rightYLabels) = partitionEithers ylabels
         leftYProviders  = experimentSeriesProviders expdata leftYLabels
         rightYProviders = experimentSeriesProviders expdata rightYLabels
         xprovider  = 
           case experimentSeriesProviders expdata [xlabel] of
             [provider] -> provider
             _ -> error $
                  "Only a single X series must be" ++
                  " provided: simulateFinalXYChart"
         providerInput providers =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateFinalXYChart"
             Just input -> (providerName provider, input)
         leftYInput  = providerInput leftYProviders
         rightYInput = providerInput rightYProviders
         yinput   = leftYInput ++ rightYInput
         [xinput] = providerInput [xprovider]
         leftYNames  = map (Left . fst) leftYInput
         rightYNames = map (Right . fst) rightYInput
         ynames = leftYNames ++ rightYNames
         xname  = fst xinput
         ys = map snd yinput
         x  = snd xinput
         predicate = finalXYChartPredicate $ finalXYChartView st
         exp = finalXYChartExperiment st
         lock = finalXYChartLock st
     results <- liftIO $ readIORef (finalXYChartResults st)
     case results of
       Nothing ->
         liftIO $
         do results <- newFinalXYChartResults xname ynames exp
            writeIORef (finalXYChartResults st) $ Just results
       Just results ->
         let diffnames = 
               (xname /= finalXYChartXName results) || 
               (ynames /= finalXYChartYNames results)
         in when diffnames $
            error "Series with different names are returned for different runs: simulateFinalXYChart"
     results <- liftIO $ fmap fromJust $ readIORef (finalXYChartResults st)
     let xys = finalXYChartXY results
         h = filterSignalM (const predicate) $
             experimentSignalInStopTime expdata
     handleSignal_ h $ \_ ->
       do x'  <- x
          ys' <- sequence ys
          i   <- liftParameter simulationIndex
          liftIO $ withMVar lock $ \() ->
            forM_ (zip ys' xys) $ \(y', xy) ->
            x' `seq` y' `seq` writeArray xy i $ Just (x', y')
     return $ return ()
     
-- | Plot the XY chart after the simulation is complete.
finaliseFinalXYChart :: ChartRenderer r => FinalXYChartViewState r -> IO ()
finaliseFinalXYChart st =
  do let title = finalXYChartTitle $ finalXYChartView st
         plotTitle = 
           replace "$TITLE" title
           (finalXYChartPlotTitle $ finalXYChartView st)
         width = finalXYChartWidth $ finalXYChartView st
         height = finalXYChartHeight $ finalXYChartView st
         plotLines = finalXYChartPlotLines $ finalXYChartView st
         plotBottomAxis = finalXYChartBottomAxis $ finalXYChartView st
         plotLayout = finalXYChartLayout $ finalXYChartView st
         renderer = finalXYChartRenderer st
     results <- readIORef $ finalXYChartResults st
     case results of
       Nothing -> return ()
       Just results ->
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
                        layoutlr_title .~ plotTitle $
                        layoutlr_plots .~ ps $
                        def
            file <- fmap (flip replaceExtension $ renderableFileExtension renderer) $
                    resolveFilePath (finalXYChartDir st) $
                    expandFilePath (finalXYChartFileName $ finalXYChartView st) $
                    M.fromList [("$TITLE", title)]
            renderChart renderer (width, height) (toRenderable chart) file
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
