
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
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
    experimentTitle = "Inspection and Adjustment Stations on a Production Line (the Monte-Carlo simulation)" }

resultProcessingTime :: ResultTransform
resultProcessingTime =
  (resultByName "inputArrivalTimer" >>>
   resultByProperty "processingTime")
  <>
  (resultByName "outputArrivalTimer" >>>
   resultByProperty "processingTime")

resultProcessingFactor :: ResultTransform
resultProcessingFactor =
  (resultByName "inspectionStations" >>>
   resultByProperty "processingFactor")
  <>
  (resultByName "adjustmentStations" >>>
   resultByProperty "processingFactor")

resultQueueSize :: ResultTransform
resultQueueSize =
  (resultByName "inspectionQueue" >>>
   resultByProperty "queueCount")
  <>
  (resultByName "adjustmentQueue" >>>
   resultByProperty "queueCount")

resultWaitTime :: ResultTransform
resultWaitTime =
  (resultByName "inspectionQueue" >>>
   resultByProperty "queueWaitTime")
  <>
  (resultByName "adjustmentQueue" >>>
   resultByProperty "queueWaitTime")

generators :: WebPageCharting r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle  = "Arrivals",
     finalStatsSeries = resultProcessingTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The processing factor (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = resultProcessingFactor },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "The processing factor (histogram)",
     finalHistogramWidth = 1000,
     finalHistogramSeries = resultProcessingFactor },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The processing factor (statistics)",
     finalStatsSeries = resultProcessingFactor },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The queue size (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = resultQueueSize },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "The queue size (histogram)",
     finalHistogramWidth = 1000,
     finalHistogramSeries = resultQueueSize },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The queue size (statistics)",
     finalStatsSeries = resultQueueSize },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The queue wait time (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = resultWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The queue wait time (statistics)",
     finalStatsSeries = resultWaitTime } ]
