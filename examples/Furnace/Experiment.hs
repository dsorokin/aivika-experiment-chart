
module Experiment (experiment) where

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
experiment :: ChartRenderer r => Experiment r
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    -- experimentRunCount = 1000,
    experimentRunCount = 100,
    experimentTitle = "The Furnace model (the Monte-Carlo simulation)",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 1",
         deviationChartPlotTitle = "The total, loaded and ready ingot counts",
         deviationChartSeries = [Right totalIngotCountName,
                                 Right loadedIngotCountName,
                                 Right readyIngotCountName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 1",
         finalHistogramPlotTitle = "The distribution of total, loaded and ready " ++
                                   "ingot counts in the final time point.",
         finalHistogramSeries = [totalIngotCountName,
                                 loadedIngotCountName,
                                 readyIngotCountName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 1",
         finalStatsDescription = "The summary of total, loaded and ready " ++
                                 "ingot counts in the final time point.",
         finalStatsSeries = [totalIngotCountName,
                             loadedIngotCountName,
                             readyIngotCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 2",
         deviationChartPlotTitle = "The used pit count",
         deviationChartSeries = [Right pitCountName] },
       
       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 2",
         finalHistogramPlotTitle = "The used pit count in the final time point.",
         finalHistogramSeries = [pitCountName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 2",
         finalStatsDescription = "The summary of the used pit count in the final time point.",
         finalStatsSeries = [pitCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 3",
         deviationChartPlotTitle = "The queue size",
         deviationChartSeries = [Right queueCountName] },
       
       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 3",
         finalHistogramPlotTitle = "The queue size in the final time point.",
         finalHistogramSeries = [queueCountName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 3",
         finalStatsDescription = "The summary of the queue size in the final time point.",
         finalStatsSeries = [queueCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 4",
         deviationChartPlotTitle = "The mean wait time",
         deviationChartSeries = [Right meanWaitTimeName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 4",
         finalStatsDescription = "The summary of the mean wait time in " ++
                                 "the final time point.",
         finalStatsSeries = [meanWaitTimeName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 5",
         deviationChartPlotTitle = "The mean heating time",
         deviationChartSeries = [Right meanHeatingTimeName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 5",
         finalStatsDescription = "The summary of the mean heating time in " ++
                                 "the final time point.",
         finalStatsSeries = [meanHeatingTimeName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 6",
         deviationChartPlotTitle = "The ready ingot temperature",
         deviationChartSeries = [Right readyIngotTempsName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 6",
         finalHistogramPlotTitle = "The ready ingot temperature in " ++
                                   "the final time point.",
         finalHistogramSeries = [readyIngotTempsName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 6",
         finalStatsDescription = "The summary of the ready ingot temperature in " ++
                                 "the final time point.",
         finalStatsSeries = [readyIngotTempsName] }
      ] }
