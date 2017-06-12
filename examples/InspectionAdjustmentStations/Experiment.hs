
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

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

inputArrivalTimer  = T.ArrivalTimer $ resultByName "inputArrivalTimer"
outputArrivalTimer = T.ArrivalTimer $ resultByName "outputArrivalTimer"

inspectionStations = T.Server $ resultByName "inspectionStations"
adjustmentStations = T.Server $ resultByName "adjustmentStations"

inspectionQueue = T.Queue $ resultByName "inspectionQueue"
adjustmentQueue = T.Queue $ resultByName "adjustmentQueue"

resultProcessingTime :: ResultTransform
resultProcessingTime =
  (T.tr $ T.arrivalProcessingTime inputArrivalTimer) <>
  (T.tr $ T.arrivalProcessingTime outputArrivalTimer)

resultProcessingFactor :: ResultTransform
resultProcessingFactor =
  (T.serverProcessingFactor inspectionStations) <>
  (T.serverProcessingFactor adjustmentStations)

inspectionQueueCount      = T.queueCount inspectionQueue
inspectionQueueCountStats = T.tr $ T.queueCountStats inspectionQueue
inspectionWaitTime        = T.tr $ T.queueWaitTime inspectionQueue

adjustmentQueueCount      = T.queueCount adjustmentQueue
adjustmentQueueCountStats = T.tr $ T.queueCountStats adjustmentQueue
adjustmentWaitTime        = T.tr $ T.queueWaitTime adjustmentQueue

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
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
     deviationChartTitle = "The inspection queue size (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = 
       inspectionQueueCount <> inspectionQueueCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The inspection queue size (statistics)",
     finalStatsSeries = inspectionQueueCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The inspection queue wait time (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = inspectionWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The inspection queue wait time (statistics)",
     finalStatsSeries = inspectionWaitTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The adjustment queue size (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = 
       adjustmentQueueCount <> adjustmentQueueCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The adjustment queue size (statistics)",
     finalStatsSeries = adjustmentQueueCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The adjustment queue wait time (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = adjustmentWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The adjustment queue wait time (statistics)",
     finalStatsSeries = adjustmentWaitTime } ]
