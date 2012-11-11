
-- |
-- Module     : Simulation.Aivika.Experiment.XYChartView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'XYChartView' that saves the XY charts 
-- in the PNG files.
--

module Simulation.Aivika.Experiment.XYChartView
       (XYChartView(..), 
        defaultXYChartView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Data.Either
import Data.Array
import Data.Monoid

import Data.Accessor

import System.IO
import System.FilePath

import Data.String.Utils (replace)

import Graphics.Rendering.Chart

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (divideBy)
import Simulation.Aivika.Experiment.Chart (colourisePlotLines)

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue

-- | Defines the 'View' that saves the XY charts
-- in the PNG files.
data XYChartView =
  XYChartView { xyChartTitle       :: String,
                -- ^ This is a title used in HTML.
                xyChartDescription :: String,
                -- ^ This is a description used in HTML.
                xyChartWidth       :: Int,
                -- ^ The width of the chart.
                xyChartHeight      :: Int,
                -- ^ The height of the chart.
                xyChartFileName    :: FileName,
                -- ^ It defines the file name for each PNG file. 
                -- It may include special variables @$TITLE@, 
                -- @$RUN_INDEX@ and @$RUN_COUNT@.
                --
                -- An example is
                --
                -- @
                --   xyChartFileName = UniqueFileName \"$TITLE - $RUN_INDEX\", \".png\"
                -- @
                xyChartPredicate   :: Dynamics Bool,
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
                xyChartLayout :: Layout1 Double Double ->
                                 Layout1 Double Double
                -- ^ A transformation of the plot layout, 
                -- where you can redefine the axes, for example.
              }
  
-- | The default time series view.  
defaultXYChartView :: XYChartView
defaultXYChartView = 
  XYChartView { xyChartTitle       = "XY Chart",
                xyChartDescription = [],
                xyChartWidth       = 640,
                xyChartHeight      = 480,
                xyChartFileName    = UniqueFileName "$TITLE - $RUN_INDEX" ".png",
                xyChartPredicate   = return True,
                xyChartXSeries     = Nothing, 
                xyChartYSeries     = [],
                xyChartPlotTitle   = "$TITLE",
                xyChartRunPlotTitle  = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                xyChartPlotLines   = colourisePlotLines,
                xyChartBottomAxis  = id,
                xyChartLayout      = id }

instance View XYChartView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newXYChart v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateXYChart st,
                               reporterTOCHtml    = xyChartTOCHtml st,
                               reporterHtml       = xyChartHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data XYChartViewState =
  XYChartViewState { xyChartView       :: XYChartView,
                     xyChartExperiment :: Experiment,
                     xyChartDir        :: FilePath, 
                     xyChartMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newXYChart :: XYChartView -> Experiment -> FilePath -> IO XYChartViewState
newXYChart view exp dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i -> 
       resolveFileName (Just dir) (xyChartFileName view) $
       M.fromList [("$TITLE", xyChartTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return XYChartViewState { xyChartView       = view,
                               xyChartExperiment = exp,
                               xyChartDir        = dir, 
                               xyChartMap        = m }
       
-- | Plot the XY chart during the simulation.
simulateXYChart :: XYChartViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateXYChart st expdata =
  do let yprotolabels = xyChartYSeries $ xyChartView st
         xprotolabel  = xyChartXSeries $ xyChartView st
         ylabels = flip map yprotolabels $ either id id
         xlabel  = flip fromMaybe xprotolabel $
                   error "X series is not provided: simulateXYChart"
         yproviders = experimentSeriesProviders expdata ylabels
         xprovider  = 
           case experimentSeriesProviders expdata [xlabel] of
             [provider] -> provider
             _ -> error $
                  "Only a single X series must be" ++
                  " provided: simulateXYChart"
         ys  = input yproviders
         [x] = input [xprovider]
         input providers =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateXYChart"
             Just input -> input
         n = experimentRunCount $ xyChartExperiment st
         width = xyChartWidth $ xyChartView st
         height = xyChartHeight $ xyChartView st
         predicate = xyChartPredicate $ xyChartView st
         plotLines = xyChartPlotLines $ xyChartView st
         plotBottomAxis = xyChartBottomAxis $ xyChartView st
         plotLayout = xyChartLayout $ xyChartView st
     i <- liftSimulation simulationIndex
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
     hs <- forM (zip yproviders ys) $ \(provider, y) ->
       let transform () =
             do p <- predicate
                if p 
                  then liftM2 (,) x y
                  else return (1/0, 1/0)  -- such values will be ignored then
       in newSignalHistoryThrough (experimentQueue expdata) $
          mapSignalM transform $
          experimentMixedSignal expdata [provider] <>
          experimentMixedSignal expdata [xprovider]
     return $
       do ps <- forM (zip3 hs yproviders plotLines) $ \(h, provider, plotLines) ->
            do (ts, zs) <- readSignalHistory h 
               return $
                 toPlot $
                 plotLines $
                 plot_lines_values ^= filterPlotLinesValues (elems zs) $
                 plot_lines_title ^= providerName provider $
                 defaultPlotLines
          let ps' = flip map (zip ps yprotolabels) $ \(p, label) ->
                case label of
                  Left _  -> Left p
                  Right _ -> Right p
              axis  = plotBottomAxis $
                      laxis_title ^= providerName xprovider $
                      defaultLayoutAxis
              chart = plotLayout $
                      layout1_bottom_axis ^= axis $
                      layout1_title ^= runPlotTitle $
                      layout1_plots ^= ps' $
                      defaultLayout1
          liftIO $ 
            do renderableToPNGFile (toRenderable chart) width height file
               when (experimentVerbose $ xyChartExperiment st) $
                 putStr "Generated file " >> putStrLn file
     
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [(Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) .
  divideBy (\(x, y) -> isNaN x || isInfinite x || isNaN y || isInfinite y)

-- | Get the HTML code.     
xyChartHtml :: XYChartViewState -> Int -> HtmlWriter ()     
xyChartHtml st index =
  let n = experimentRunCount $ xyChartExperiment st
  in if n == 1
     then xyChartHtmlSingle st index
     else xyChartHtmlMultiple st index
     
-- | Get the HTML code for a single run.
xyChartHtmlSingle :: XYChartViewState -> Int -> HtmlWriter ()
xyChartHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (xyChartMap st)
     writeHtmlParagraph $
       writeHtmlImage (makeRelative (xyChartDir st) f)

-- | Get the HTML code for multiple runs.
xyChartHtmlMultiple :: XYChartViewState -> Int -> HtmlWriter ()
xyChartHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ xyChartExperiment st
     forM_ [0..(n - 1)] $ \i ->
       let f = fromJust $ M.lookup i (xyChartMap st)
       in writeHtmlParagraph $
          writeHtmlImage (makeRelative (xyChartDir st) f)

header :: XYChartViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (xyChartTitle $ xyChartView st)
     let description = xyChartDescription $ xyChartView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
xyChartTOCHtml :: XYChartViewState -> Int -> HtmlWriter ()
xyChartTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (xyChartTitle $ xyChartView st)
