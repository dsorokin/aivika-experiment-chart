
-- |
-- Module     : Simulation.Aivika.Experiment.FinalHistogramView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'FinalHistogramView' that draws a histogram
-- by the specified series in final time points collected from different 
-- simulation runs.
--

module Simulation.Aivika.Experiment.FinalHistogramView
       (FinalHistogramView(..), 
        defaultFinalHistogramView) where

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
import Simulation.Aivika.Experiment.Chart (colourisePlotBars)
import Simulation.Aivika.Experiment.Histogram

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base (starttime, integIterationBnds, integTimes, integIteration)

-- | Defines the 'View' that saves the histogram
-- in the PNG file by the specified series in
-- final time points collected from different
-- simulation runs.
data FinalHistogramView =
  FinalHistogramView { finalHistogramTitle       :: String,
                       -- ^ This is a title used in HTML.
                       finalHistogramDescription :: String,
                       -- ^ This is a description used in HTML.
                       finalHistogramWidth       :: Int,
                       -- ^ The width of the histogram.
                       finalHistogramHeight      :: Int,
                       -- ^ The height of the histogram.
                       finalHistogramFileName    :: FileName,
                       -- ^ It defines the file name for the PNG file. 
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   finalHistogramFileName = UniqueFileName \"$TITLE\", \".png\"
                       -- @
                       finalHistogramPredicate   :: Dynamics Bool,
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
                       finalHistogramLayout :: Layout1 Double Double ->
                                               Layout1 Double Double
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
                       finalHistogramFileName    = UniqueFileName "$TITLE" ".png",
                       finalHistogramPredicate   = return True,
                       finalHistogramBuild       = histogram binSturges,
                       finalHistogramSeries      = [], 
                       finalHistogramPlotTitle   = "$TITLE",
                       finalHistogramPlotBars    = colourisePlotBars,
                       finalHistogramLayout      = id }

instance View FinalHistogramView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newFinalHistogram v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = finaliseFinalHistogram st,
                               reporterSimulate   = simulateFinalHistogram st,
                               reporterTOCHtml    = finalHistogramTOCHtml st,
                               reporterHtml       = finalHistogramHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data FinalHistogramViewState =
  FinalHistogramViewState { finalHistogramView       :: FinalHistogramView,
                            finalHistogramExperiment :: Experiment,
                            finalHistogramDir        :: FilePath, 
                            finalHistogramFile       :: IORef (Maybe FilePath),
                            finalHistogramLock       :: MVar (),
                            finalHistogramResults    :: IORef (Maybe FinalHistogramResults) }

-- | The histogram item.
data FinalHistogramResults =
  FinalHistogramResults { finalHistogramNames  :: [String],
                          finalHistogramValues :: [IORef [Double]] }
  
-- | Create a new state of the view.
newFinalHistogram :: FinalHistogramView -> Experiment -> FilePath -> IO FinalHistogramViewState
newFinalHistogram view exp dir =
  do f <- newIORef Nothing
     l <- newMVar () 
     r <- newIORef Nothing
     return FinalHistogramViewState { finalHistogramView       = view,
                                      finalHistogramExperiment = exp,
                                      finalHistogramDir        = dir, 
                                      finalHistogramFile       = f,
                                      finalHistogramLock       = l, 
                                      finalHistogramResults    = r }
       
-- | Create new histogram results.
newFinalHistogramResults :: [String] -> Experiment -> IO FinalHistogramResults
newFinalHistogramResults names exp =
  do values <- forM names $ \_ -> liftIO $ newIORef []
     return FinalHistogramResults { finalHistogramNames  = names,
                                    finalHistogramValues = values }
       
-- | Simulation of the specified series.
simulateFinalHistogram :: FinalHistogramViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateFinalHistogram st expdata =
  do let protolabels = finalHistogramSeries $ finalHistogramView st
         protoproviders = flip map protolabels $ \protolabel ->
           experimentSeriesProviders expdata [protolabel]
         providers = concat protoproviders
         input =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateFinalHistogram"
             Just input -> input
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
     t0 <- starttime
     enqueue (experimentQueue expdata) t0 $
       do let h = filterSignalM (const predicate) $
                  experimentSignalInStopTime expdata
          -- we must subscribe through the event queue;
          -- otherwise, we will loose a signal in the start time,
          -- because the handleSignal_ function checks the event queue
          handleSignal_ h $ \_ ->
            do xs <- sequence input
               liftIO $ withMVar lock $ \() ->
                 forM_ (zip xs values) $ \(x, values) ->
                 x `seq` modifyIORef values (x :)
     return $ return ()
     
-- | Plot the histogram after the simulation is complete.
finaliseFinalHistogram :: FinalHistogramViewState -> IO ()
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
     results <- readIORef $ finalHistogramResults st
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = finalHistogramNames results
                values = finalHistogramValues results
            xs <- forM values readIORef
            let zs = histogramToBars . filterHistogram . histogram $ 
                     map filterData xs
                p  = plotBars $
                     bars $
                     plot_bars_values ^= zs $
                     plot_bars_titles ^= names $
                     defaultPlotBars
            let chart = layout $
                        layout1_title ^= plotTitle $
                        layout1_plots ^= [Left p] $
                        defaultLayout1
            file <- resolveFileName 
                    (Just $ finalHistogramDir st)
                    (finalHistogramFileName $ finalHistogramView st) $
                    M.fromList [("$TITLE", title)]
            renderableToPNGFile (toRenderable chart) width height file
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
finalHistogramHtml :: FinalHistogramViewState -> Int -> HtmlWriter ()
finalHistogramHtml st index =
  do header st index
     file <- liftIO $ readIORef (finalHistogramFile st)
     case file of
       Nothing -> return ()
       Just f  ->
         writeHtmlParagraph $
         writeHtmlImage (makeRelative (finalHistogramDir st) f)

header :: FinalHistogramViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalHistogramTitle $ finalHistogramView st)
     let description = finalHistogramDescription $ finalHistogramView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalHistogramTOCHtml :: FinalHistogramViewState -> Int -> HtmlWriter ()
finalHistogramTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalHistogramTitle $ finalHistogramView st)
