
module Experiment (experiment) where

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0, 
                spcStopTime = 500, 
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: ChartRenderer r => Experiment r
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Linear Array",
    experimentDescription = "Model Linear Array as described in " ++
                            "the examples included in Berkeley-Madonna.",
    experimentGenerators = 
      [outputView defaultExperimentSpecsView,
       outputView $ defaultTableView {
         tableSeries = ["t", "m", "c"] },
       outputView $ defaultTimeSeriesView {
         timeSeries = [Left "m"],
         timeSeriesWidth = 800,
         timeSeriesHeight = 800 },
       outputView $ defaultTimeSeriesView {
         timeSeries = [Right "c"],
         timeSeriesWidth = 800,
         timeSeriesHeight = 800 },
       outputView $ defaultTimeSeriesView {
         timeSeries = [Left "m", Right "c"],
         timeSeriesWidth = 800,
         timeSeriesHeight = 800 },
       outputView $ defaultXYChartView {
         xyChartXSeries = Just "t",
         xyChartYSeries = [Left "m"],
         xyChartWidth = 800,
         xyChartHeight = 800 },
       outputView $ defaultXYChartView {
         xyChartXSeries = Just "t",
         xyChartYSeries = [Right "c"],
         xyChartWidth = 800,
         xyChartHeight = 800 },
       outputView $ defaultXYChartView {
         xyChartXSeries = Just "t",
         xyChartYSeries = [Left "m", Right "c"],
         xyChartWidth = 800,
         xyChartHeight = 800 }
       ] }
