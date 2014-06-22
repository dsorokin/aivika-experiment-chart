
module Experiment (experiment) where

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: ChartRenderer r => Experiment r
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Chemical Reaction",
    experimentDescription = "Chemical Reaction as described in " ++
                            "the 5-minute tutorial of Berkeley-Madonna",
    experimentGenerators = 
      [outputView defaultExperimentSpecsView,
       outputView $ defaultLastValueView {
         lastValueSeries = ["t", "a", "b", "c"] },
       outputView $ defaultTableView {
         tableSeries = ["t", "a", "b", "c"] }, 
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time Series",
         timeSeries = [Left "a", Left "b", Left "c"] },
       outputView $ defaultXYChartView {
         xyChartTitle = "XYChart - 1",
         xyChartPlotTitle = "b=b(a), c=c(a)",
         xyChartXSeries = Just "a",
         xyChartYSeries = [Left "b", Right "c"] },
       outputView $ defaultXYChartView {
         xyChartTitle = "XYChart - 2",
         xyChartPlotTitle = "a=a(b), c=c(b)",
         xyChartXSeries = Just "b",
         xyChartYSeries = [Right "a", Right "c"] },
       outputView $ defaultXYChartView {
         xyChartTitle = "XYChart - 3",
         xyChartPlotTitle = "a=a(c), b=b(c)",
         xyChartXSeries = Just "c",
         xyChartYSeries = [Right "a", Left "b"] } ] }
