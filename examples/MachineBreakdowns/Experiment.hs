
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

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

jobsCompleted :: ResultTransform
jobsCompleted = 
  resultByName "jobsCompleted" >>>
  resultById ArrivalProcessingTimeId >>>
  expandResults >>>
  resultById SamplingStatsCountId

processingTime :: ResultTransform
processingTime = 
  resultByName "jobsCompleted" >>>
  resultById ArrivalProcessingTimeId

jobsInterrupted :: ResultTransform
jobsInterrupted = resultByName "jobsInterrupted"

waitTime :: ResultTransform
waitTime = 
  resultByName "inputQueue" >>> 
  resultById QueueWaitTimeId

queueSize :: ResultTransform
queueSize = 
  resultByName "inputQueue" >>> 
  resultById QueueCountStatsId

processingFactor :: ResultTransform
processingFactor =
  resultByName "machineProcessing" >>>
  resultById ServerProcessingFactorId

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle  = "Machine Tool With Breakdowns",
     finalStatsSeries = jobsCompleted <> jobsInterrupted },
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
     deviationChartRightYSeries = queueSize },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Queue Size (statistics)",
     finalStatsSeries = queueSize },
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
