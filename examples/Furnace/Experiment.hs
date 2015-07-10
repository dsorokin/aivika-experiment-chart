
module Experiment (experiment, generators) where

import Data.Monoid
import Control.Category

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

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

inputIngotCount  = resultByName inputIngotCountName
loadedIngotCount = resultByName loadedIngotCountName
outputIngotCount = resultByName outputIngotCountName
pitCount         = resultByName pitCountName
heatingTime      = resultByName heatingTimeName
outputIngotTemp  = resultByName outputIngotTempName

furnaceQueue     = T.Queue $ resultByName furnaceQueueName
furnaceQueueCount      = T.tr $ T.queueCount furnaceQueue
furnaceQueueCountStats = T.tr $ T.queueCountStats furnaceQueue
furnaceQueueWaitTime   = T.tr $ T.queueWaitTime furnaceQueue
furnaceQueueRate       = T.tr $ T.queueRate furnaceQueue

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 1",
     deviationChartPlotTitle = "The input, loaded and output ingot counts",
     deviationChartRightYSeries =
       inputIngotCount <> loadedIngotCount <> outputIngotCount },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 1",
     finalHistogramPlotTitle = "The distribution of input, loaded and output " ++
                               "ingot counts in the final time point.",
     finalHistogramSeries =
       inputIngotCount <> loadedIngotCount <> outputIngotCount },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 1",
     finalStatsDescription = "The summary of input, loaded and output " ++
                             "ingot counts in the final time point.",
     finalStatsSeries =
       inputIngotCount <> loadedIngotCount <> outputIngotCount },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 2",
     deviationChartPlotTitle = "The used pit count",
     deviationChartRightYSeries = pitCount },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 2",
     finalHistogramPlotTitle = "The used pit count in the final time point.",
     finalHistogramSeries = pitCount },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 2",
     finalStatsDescription = "The summary of the used pit count in the final time point.",
     finalStatsSeries = pitCount },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 3",
     deviationChartPlotTitle = "The queue size",
     deviationChartRightYSeries = furnaceQueueCount <> furnaceQueueCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 3",
     finalStatsDescription = "The summary of the average queue size in the final time point.",
     finalStatsSeries = furnaceQueueCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 4",
     deviationChartPlotTitle = "The mean wait time",
     deviationChartRightYSeries = furnaceQueueWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 4",
     finalStatsDescription = "The summary of the mean wait time in " ++
                             "the final time point.",
     finalStatsSeries = furnaceQueueWaitTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 5",
     deviationChartPlotTitle = "The queue rate",
     deviationChartRightYSeries = furnaceQueueRate },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 5",
     finalStatsDescription = "The summary of the queue rate in " ++
                             "the final time point.",
     finalStatsSeries = furnaceQueueRate },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 6",
     deviationChartPlotTitle = "The mean heating time",
     deviationChartRightYSeries = heatingTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 6",
     finalStatsDescription = "The summary of the mean heating time in " ++
                             "the final time point.",
     finalStatsSeries = heatingTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Deviation Chart - 7",
     deviationChartPlotTitle = "The output ingot temperature",
     deviationChartRightYSeries = outputIngotTemp },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Final Histogram - 7",
     finalHistogramPlotTitle = "The output ingot temperature in " ++
                               "the final time point.",
     finalHistogramSeries = outputIngotTemp },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Final Statistics - 7",
     finalStatsDescription = "The summary of the output ingot temperature in " ++
                             "the final time point.",
     finalStatsSeries = outputIngotTemp } ]
