
module Experiment (experiment, generators) where

import Data.Monoid
import Control.Category

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import Model

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                -- spcStopTime = 300.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    -- experimentRunCount = 1000,
    experimentRunCount = 100,
    experimentTitle = "The Furnace model (the Monte-Carlo simulation)" }

generators :: WebPageCharting r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 1",
     deviationChartPlotTitle = "The input, loaded and output ingot counts",
     deviationChartRightYSeries =
       resultByName inputIngotCountName <>
       resultByName loadedIngotCountName <>
       resultByName outputIngotCountName },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 1",
     finalHistogramPlotTitle = "The distribution of input, loaded and output " ++
                               "ingot counts in the final time point.",
     finalHistogramSeries =
       resultByName inputIngotCountName <>
       resultByName loadedIngotCountName <>
       resultByName outputIngotCountName },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 1",
     finalStatsDescription = "The summary of input, loaded and output " ++
                             "ingot counts in the final time point.",
     finalStatsSeries =
       resultByName inputIngotCountName <>
       resultByName loadedIngotCountName <>
       resultByName outputIngotCountName },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 2",
     deviationChartPlotTitle = "The used pit count",
     deviationChartRightYSeries =
       resultByName pitCountName },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 2",
     finalHistogramPlotTitle = "The used pit count in the final time point.",
     finalHistogramSeries =
       resultByName pitCountName },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 2",
     finalStatsDescription = "The summary of the used pit count in the final time point.",
     finalStatsSeries =
       resultByName pitCountName },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 3",
     deviationChartPlotTitle = "The queue size",
     deviationChartRightYSeries =
       resultByName furnaceQueueName >>> resultById QueueCountId },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 3",
     finalHistogramPlotTitle = "The queue size in the final time point.",
     finalHistogramSeries =
       resultByName furnaceQueueName >>> resultById QueueCountId },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 3",
     finalStatsDescription = "The summary of the queue size in the final time point.",
     finalStatsSeries =
       resultByName furnaceQueueName >>> resultById QueueCountId },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 4",
     deviationChartPlotTitle = "The mean wait time",
     deviationChartRightYSeries =
       resultByName furnaceQueueName >>> resultById QueueWaitTimeId },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 4",
     finalStatsDescription = "The summary of the mean wait time in " ++
                             "the final time point.",
     finalStatsSeries =
       resultByName furnaceQueueName >>> resultById QueueWaitTimeId },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 5",
     deviationChartPlotTitle = "The queue rate",
     deviationChartRightYSeries =
       resultByName furnaceQueueName >>> resultById QueueRateId },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 5",
     finalStatsDescription = "The summary of the queue rate in " ++
                             "the final time point.",
     finalStatsSeries =
       resultByName furnaceQueueName >>> resultById QueueRateId },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 6",
     deviationChartPlotTitle = "The mean heating time",
     deviationChartRightYSeries =
       resultByName heatingTimeName },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 6",
     finalStatsDescription = "The summary of the mean heating time in " ++
                             "the final time point.",
     finalStatsSeries =
       resultByName heatingTimeName },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 7",
     deviationChartPlotTitle = "The output ingot temperature",
     deviationChartRightYSeries =
       resultByName outputIngotTempName },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 7",
     finalHistogramPlotTitle = "The output ingot temperature in " ++
                               "the final time point.",
     finalHistogramSeries =
       resultByName outputIngotTempName },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 7",
     finalStatsDescription = "The summary of the output ingot temperature in " ++
                             "the final time point.",
     finalStatsSeries =
       resultByName outputIngotTempName } ]
