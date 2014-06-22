
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.FinalHistogramView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'FinalHistogramView' that draws a histogram
-- by the specified series in final time points collected from different 
-- simulation runs.
--

module Simulation.Aivika.Experiment.Chart.FinalHistogramView
       (FinalHistogramView(..), 
        defaultFinalHistogramView) where

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
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotBars)
import Simulation.Aivika.Experiment.Histogram
import Simulation.Aivika.Experiment.ListSource

import Simulation.Aivika.Specs
import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Signal

-- | Defines the 'View' that saves the histogram
-- for the specified series in final time points
-- collected from different simulation runs.
data FinalHistogramView =
  FinalHistogramView { finalHistogramTitle       :: String,
                       -- ^ This is a title used in HTML.
                       finalHistogramDescription :: String,
                       -- ^ This is a description used in HTML.
                       finalHistogramWidth       :: Int,
                       -- ^ The width of the histogram.
                       finalHistogramHeight      :: Int,
                       -- ^ The height of the histogram.
                       finalHistogramFileName    :: ExperimentFilePath,
                       -- ^ It defines the file name with optional extension for each image to be saved.
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   finalHistogramFileName = UniqueFilePath \"$TITLE\"
                       -- @
                       finalHistogramPredicate   :: Event Bool,
                       -- ^ It specifies the predicate that defines
                       -- when we count data when plotting the histogram.
                       finalHistogramBuild       :: [[Double]] -> Histogram, 
                       -- ^ Builds a histogram by the specified list of 
                       -- data series.
                       finalHistogramSeries      :: [String],
                       -- ^ It contains the labels of data plotted
                       -- on the histogram.
                       finalHistogramPlotTitle   :: String,
                       -- ^ This is a title used in the histogram. 
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   finalHistogramPlotTitle = \"$TITLE\"
                       -- @
                       finalHistogramPlotBars :: PlotBars Double Double ->
                                                 PlotBars Double Double,
                       -- ^ A transformation based on which the plot bar
                       -- is constructed for the series. 
                       --
                       -- Here you can define a colour or style of
                       -- the plot bars.
                       finalHistogramLayout :: Layout Double Double ->
                                               Layout Double Double
                       -- ^ A transformation of the plot layout, 
                       -- where you can redefine the axes, for example.
                 }
  
-- | The default histogram view.  
defaultFinalHistogramView :: FinalHistogramView
defaultFinalHistogramView = 
  FinalHistogramView { finalHistogramTitle       = "Final Histogram",
                       finalHistogramDescription = "It shows a histogram by data gathered in the final time points.",
                       finalHistogramWidth       = 640,
                       finalHistogramHeight      = 480,
                       finalHistogramFileName    = UniqueFilePath "$TITLE",
                       finalHistogramPredicate   = return True,
                       finalHistogramBuild       = histogram binSturges,
                       finalHistogramSeries      = [], 
                       finalHistogramPlotTitle   = "$TITLE",
                       finalHistogramPlotBars    = colourisePlotBars,
                       finalHistogramLayout      = id }

instance ChartRenderer r => ExperimentView FinalHistogramView r where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newFinalHistogram v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalHistogram st,
                                         reporterSimulate   = simulateFinalHistogram st,
                                         reporterTOCHtml    = finalHistogramTOCHtml st,
                                         reporterHtml       = finalHistogramHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalHistogramViewState r =
  FinalHistogramViewState { finalHistogramView       :: FinalHistogramView,
                            finalHistogramExperiment :: Experiment r,
                            finalHistogramRenderer   :: r,
                            finalHistogramDir        :: FilePath, 
                            finalHistogramFile       :: IORef (Maybe FilePath),
                            finalHistogramLock       :: MVar (),
                            finalHistogramResults    :: IORef (Maybe FinalHistogramResults) }

-- | The histogram item.
data FinalHistogramResults =
  FinalHistogramResults { finalHistogramNames  :: [String],
                          finalHistogramValues :: [ListRef Double] }
  
-- | Create a new state of the view.
newFinalHistogram :: FinalHistogramView -> Experiment r -> r -> FilePath -> IO (FinalHistogramViewState r)
newFinalHistogram view exp renderer dir =
  do f <- newIORef Nothing
     l <- newMVar () 
     r <- newIORef Nothing
     return FinalHistogramViewState { finalHistogramView       = view,
                                      finalHistogramExperiment = exp,
                                      finalHistogramRenderer   = renderer,
                                      finalHistogramDir        = dir, 
                                      finalHistogramFile       = f,
                                      finalHistogramLock       = l, 
                                      finalHistogramResults    = r }
       
-- | Create new histogram results.
newFinalHistogramResults :: [String] -> Experiment r -> IO FinalHistogramResults
newFinalHistogramResults names exp =
  do values <- forM names $ \_ -> liftIO newListRef
     return FinalHistogramResults { finalHistogramNames  = names,
                                    finalHistogramValues = values }
       
-- | Simulation of the specified series.
simulateFinalHistogram :: FinalHistogramViewState r -> ExperimentData -> Event (Event ())
simulateFinalHistogram st expdata =
  do let labels = finalHistogramSeries $ finalHistogramView st
         providers = experimentSeriesProviders expdata labels
         input =
           flip map providers $ \provider ->
           case providerToDoubleListSource provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as a source of double values: simulateFinalHistogram"
             Just input -> listSourceData input
         names = map providerName providers
         predicate = finalHistogramPredicate $ finalHistogramView st
         exp = finalHistogramExperiment st
         lock = finalHistogramLock st
     results <- liftIO $ readIORef (finalHistogramResults st)
     case results of
       Nothing ->
         liftIO $
         do results <- newFinalHistogramResults names exp
            writeIORef (finalHistogramResults st) $ Just results
       Just results ->
         when (names /= finalHistogramNames results) $
         error "Series with different names are returned for different runs: simulateFinalHistogram"
     results <- liftIO $ fmap fromJust $ readIORef (finalHistogramResults st)
     let values = finalHistogramValues results
         h = filterSignalM (const predicate) $
             experimentSignalInStopTime expdata
     handleSignal_ h $ \_ ->
       do xs <- sequence input
          liftIO $ withMVar lock $ \() ->
            forM_ (zip xs values) $ \(x, values) ->
            addDataToListRef values x
     return $ return ()
     
-- | Plot the histogram after the simulation is complete.
finaliseFinalHistogram :: ChartRenderer r => FinalHistogramViewState r -> IO ()
finaliseFinalHistogram st =
  do let title = finalHistogramTitle $ finalHistogramView st
         plotTitle = 
           replace "$TITLE" title
           (finalHistogramPlotTitle $ finalHistogramView st)
         width = finalHistogramWidth $ finalHistogramView st
         height = finalHistogramHeight $ finalHistogramView st
         histogram = finalHistogramBuild $ finalHistogramView st
         bars = finalHistogramPlotBars $ finalHistogramView st
         layout = finalHistogramLayout $ finalHistogramView st
         renderer = finalHistogramRenderer st
     results <- readIORef $ finalHistogramResults st
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = finalHistogramNames results
                values = finalHistogramValues results
            xs <- forM values readListRef
            let zs = histogramToBars . filterHistogram . histogram $ 
                     map filterData xs
                p  = plotBars $
                     bars $
                     plot_bars_values .~ zs $
                     plot_bars_titles .~ names $
                     def
                updateAxes =
                  if null zs
                  then let v = AxisVisibility True False False
                       in \l -> layout_top_axis_visibility .~ v $
                                layout_bottom_axis_visibility .~ v $
                                layout_left_axis_visibility .~ v $
                                layout_right_axis_visibility .~ v $
                                l
                  else id
                chart = layout . updateAxes $
                        layout_title .~ plotTitle $
                        layout_plots .~ [p] $
                        def
            file <- resolveFilePath (finalHistogramDir st) $
                    mapFilePath (flip replaceExtension $ renderableFileExtension renderer) $
                    expandFilePath (finalHistogramFileName $ finalHistogramView st) $
                    M.fromList [("$TITLE", title)]
            renderChart renderer (width, height) (toRenderable chart) file
            when (experimentVerbose $ finalHistogramExperiment st) $
              putStr "Generated file " >> putStrLn file
            writeIORef (finalHistogramFile st) $ Just file
     
-- | Remove the NaN and inifity values.     
filterData :: [Double] -> [Double]
filterData = filter (\x -> not $ isNaN x || isInfinite x)
     
-- | Remove the NaN and inifity values.     
filterHistogram :: [(Double, a)] -> [(Double, a)]
filterHistogram = filter (\(x, _) -> not $ isNaN x || isInfinite x)

-- | Convert a histogram to the bars.
histogramToBars :: [(Double, [Int])] -> [(Double, [Double])]
histogramToBars = map $ \(x, ns) -> (x, map fromIntegral ns)

-- | Get the HTML code.     
finalHistogramHtml :: FinalHistogramViewState r -> Int -> HtmlWriter ()
finalHistogramHtml st index =
  do header st index
     file <- liftIO $ readIORef (finalHistogramFile st)
     case file of
       Nothing -> return ()
       Just f  ->
         writeHtmlParagraph $
         writeHtmlImage (makeRelative (finalHistogramDir st) f)

header :: FinalHistogramViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalHistogramTitle $ finalHistogramView st)
     let description = finalHistogramDescription $ finalHistogramView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalHistogramTOCHtml :: FinalHistogramViewState r -> Int -> HtmlWriter ()
finalHistogramTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalHistogramTitle $ finalHistogramView st)
