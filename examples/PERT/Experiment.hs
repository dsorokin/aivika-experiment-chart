
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 10000,
    -- experimentRunCount = 10,
    experimentTitle = "Analysis of a PERT-type Network" }

timers = resultByName "timers"

timer2 = timers >>> resultByIndex 0
timer3 = timers >>> resultByIndex 1
timer4 = timers >>> resultByIndex 2
timer5 = timers >>> resultByIndex 3

projCompletion = resultByName "projCompletion"

completionTime series = 
  T.tr $ 
  T.arrivalProcessingTime $
  T.ArrivalTimer series

completionTimeMean series = 
  T.tr $ 
  T.samplingStatsMean $
  T.arrivalProcessingTime $
  T.ArrivalTimer series

histogramView title series = 
  defaultFinalHistogramView {
    finalHistogramTitle  = title,
    finalHistogramSeries = series
  }

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Completion Time",
     finalStatsSeries = 
       completionTime timers <> 
       completionTime projCompletion },
   outputView $ histogramView "Node 2" $ completionTimeMean timer2,
   outputView $ histogramView "Node 3" $ completionTimeMean timer3,
   outputView $ histogramView "Node 4" $ completionTimeMean timer3,
   outputView $ histogramView "Node 5" $ completionTimeMean timer5,
   outputView $ histogramView "The Project Completion" $ 
     completionTimeMean projCompletion]
