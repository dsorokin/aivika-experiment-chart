
module Experiment (experiment) where

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
experiment :: ChartRenderer r => Experiment r
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    -- experimentRunCount = 10,
    experimentTitle = "Inspection and Adjustment Stations on a Production Line (the Monte-Carlo simulation)",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Arrivals",
         finalStatsSeries = ["incomingArrivalCount",
                             "outgoingArrivalCount",
                             "outgoingArrivalTimer"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The processing factor (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "inspectionStationProcessingFactor",
                                 Right "adjustmentStationProcessingFactor"] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "The processing factor (histogram)",
         finalHistogramWidth = 1000,
         finalHistogramSeries = ["inspectionStationProcessingFactor",
                                 "adjustmentStationProcessingFactor"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The processing factor (statistics)",
         finalStatsSeries = ["inspectionStationProcessingFactor",
                             "adjustmentStationProcessingFactor"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The queue size (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "inspectionQueueSize",
                                 Right "adjustmentQueueSize"] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "The queue size (histogram)",
         finalHistogramWidth = 1000,
         finalHistogramSeries = ["inspectionQueueSize",
                                 "adjustmentQueueSize"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The queue size (statistics)",
         finalStatsSeries = ["inspectionQueueSize",
                             "adjustmentQueueSize"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The queue wait time (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "inspectionQueueWaitTime",
                                 Right "adjustmentQueueWaitTime"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The queue wait time (statistics)",
         finalStatsSeries = ["inspectionQueueWaitTime",
                             "adjustmentQueueWaitTime"] } ] }
