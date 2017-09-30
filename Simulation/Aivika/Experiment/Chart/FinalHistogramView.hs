
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Chart.FinalHistogramView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'FinalHistogramView' that plots a histogram
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
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotBars)

-- | Defines the 'View' that plots the histogram
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
                       finalHistogramTransform   :: ResultTransform,
                       -- ^ The transform applied to the results before receiving series.
                       finalHistogramSeries      :: ResultTransform, 
                       -- ^ It defines the series to be plotted on the histogram.
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
                       finalHistogramFileName    = UniqueFilePath "FinalHistogram",
                       finalHistogramPredicate   = return True,
                       finalHistogramBuild       = histogram binSturges,
                       finalHistogramTransform   = id,
                       finalHistogramSeries      = mempty, 
                       finalHistogramPlotTitle   = "$TITLE",
                       finalHistogramPlotBars    = colourisePlotBars,
                       finalHistogramLayout      = id }

instance ChartRendering r => ExperimentView FinalHistogramView (WebPageRenderer r) where
  
  outputView v = 
    let reporter exp (WebPageRenderer renderer _) dir =
          do st <- newFinalHistogram v exp renderer dir
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = finalHistogramTOCHtml st,
                                   reporterWriteHtml    = finalHistogramHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalHistogram st,
                                         reporterSimulate   = simulateFinalHistogram st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }

instance ChartRendering r => ExperimentView FinalHistogramView (FileRenderer r) where
  
  outputView v = 
    let reporter exp (FileRenderer renderer _) dir =
          do st <- newFinalHistogram v exp renderer dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalHistogram st,
                                         reporterSimulate   = simulateFinalHistogram st,
                                         reporterContext    = FileContext }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalHistogramViewState r =
  FinalHistogramViewState { finalHistogramView       :: FinalHistogramView,
                            finalHistogramExperiment :: Experiment,
                            finalHistogramRenderer   :: r,
                            finalHistogramDir        :: FilePath, 
                            finalHistogramFile       :: IORef (Maybe FilePath),
                            finalHistogramResults    :: MVar (Maybe FinalHistogramResults) }

-- | The histogram item.
data FinalHistogramResults =
  FinalHistogramResults { finalHistogramNames  :: [String],
                          finalHistogramValues :: [MVar [Double]] }
  
-- | Create a new state of the view.
newFinalHistogram :: FinalHistogramView -> Experiment -> r -> FilePath -> ExperimentWriter (FinalHistogramViewState r)
newFinalHistogram view exp renderer dir =
  liftIO $
  do f <- newIORef Nothing
     r <- newMVar Nothing
     return FinalHistogramViewState { finalHistogramView       = view,
                                      finalHistogramExperiment = exp,
                                      finalHistogramRenderer   = renderer,
                                      finalHistogramDir        = dir, 
                                      finalHistogramFile       = f,
                                      finalHistogramResults    = r }
       
-- | Create new histogram results.
newFinalHistogramResults :: [String] -> Experiment -> IO FinalHistogramResults
newFinalHistogramResults names exp =
  do values <- forM names $ \_ -> liftIO $ newMVar []
     return FinalHistogramResults { finalHistogramNames  = names,
                                    finalHistogramValues = values }
       
-- | Require to return unique results associated with the specified state. 
requireFinalHistogramResults :: FinalHistogramViewState r -> [String] -> IO FinalHistogramResults
requireFinalHistogramResults st names =
  maybePutMVar (finalHistogramResults st)
  (newFinalHistogramResults names (finalHistogramExperiment st)) $ \results ->
  if (names /= finalHistogramNames results)
  then error "Series with different names are returned for different runs: requireFinalHistogramResults"
  else return results

-- | Simulation of the specified series.
simulateFinalHistogram :: FinalHistogramViewState r -> ExperimentData -> Composite ()
simulateFinalHistogram st expdata =
  do let view    = finalHistogramView st
         loc     = localisePathResultTitle $
                   experimentLocalisation $
                   finalHistogramExperiment st
         rs      = finalHistogramSeries view $
                   finalHistogramTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleListValues rs
         names   = map (loc . resultValueIdPath) exts
         signals = experimentPredefinedSignals expdata
         signal  = filterSignalM (const predicate) $
                   resultSignalInStopTime signals
         predicate = finalHistogramPredicate view
     results <- liftIO $ requireFinalHistogramResults st names
     let values = finalHistogramValues results
     handleSignalComposite signal $ \_ ->
       do xs <- forM exts resultValueData
          liftIO $
            forM_ (zip xs values) $ \(x, values) ->
            modifyMVar_ values $ return . (++) x
     
-- | Plot the histogram after the simulation is complete.
finaliseFinalHistogram :: ChartRendering r => FinalHistogramViewState r -> ExperimentWriter ()
finaliseFinalHistogram st =
  do let view = finalHistogramView st
         title = finalHistogramTitle view
         plotTitle = finalHistogramPlotTitle view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         width = finalHistogramWidth view
         height = finalHistogramHeight view
         histogram = finalHistogramBuild view
         bars = finalHistogramPlotBars view
         layout = finalHistogramLayout view
         renderer = finalHistogramRenderer st
     file <- resolveFilePath (finalHistogramDir st) $
             mapFilePath (flip replaceExtension $ renderableChartExtension renderer) $
             expandFilePath (finalHistogramFileName view) $
             M.fromList [("$TITLE", title)]
     results <- liftIO $ readMVar $ finalHistogramResults st
     case results of
       Nothing -> return ()
       Just results ->
         liftIO $
         do let names  = finalHistogramNames results
                values = finalHistogramValues results
            xs <- forM values readMVar
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
                chart = layout .
                        renderingLayout renderer .
                        updateAxes $
                        layout_title .~ plotTitle' $
                        layout_plots .~ [p] $
                        def
            renderChart renderer (width, height) file (toRenderable chart)
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
