
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 500.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    -- experimentRunCount = 10,
    experimentTitle = "Machine Tool with Breakdowns" }

jobsCompleted     = T.ArrivalTimer $ resultByName "jobsCompleted"
jobsInterrupted   = resultByName "jobsInterrupted"
inputQueue        = T.Queue $ resultByName "inputQueue"
machineProcessing = T.Server $ resultByName "machineProcessing"

jobsCompletedCount =
  T.tr $ T.samplingStatsCount $
  T.arrivalProcessingTime jobsCompleted
  
processingTime :: ResultTransform
processingTime =
  T.tr $ T.arrivalProcessingTime jobsCompleted

waitTime :: ResultTransform
waitTime =
  T.tr $ T.queueWaitTime inputQueue

queueCount :: ResultTransform
queueCount =
  T.tr $ T.queueCount inputQueue

queueCountStats :: ResultTransform
queueCountStats =
  T.tr $ T.queueCountStats inputQueue

processingFactor :: ResultTransform
processingFactor =
  T.tr $ T.serverProcessingFactor machineProcessing

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle  = "Machine Tool With Breakdowns",
     finalStatsSeries = jobsCompletedCount <> jobsInterrupted },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Wait Time (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = waitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Wait Time (statistics)",
     finalStatsSeries = waitTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Queue Size (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = queueCount <> queueCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Queue Size (statistics)",
     finalStatsSeries = queueCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Processing Time (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = processingTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Processing Time (statistics)",
     finalStatsSeries = processingTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Machine Load (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = processingFactor },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "The Machine Load (histogram)",
     finalHistogramWidth = 1000,
     finalHistogramSeries = processingFactor },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Machine Load (statistics)",
     finalStatsSeries = processingFactor } ]
