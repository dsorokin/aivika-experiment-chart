
module Experiment (experiment, generators) where

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Chemical Reaction",
    experimentDescription = "Chemical Reaction as described in " ++
                            "the 5-minute tutorial of Berkeley-Madonna" }

generators :: WebPageCharting r => [ExperimentGenerator r WebPageWriter]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultLastValueView {
     lastValueSeries =
        resultByName "t" <>
        resultByName "a" <>
        resultByName "b" <>
        resultByName "c" },
   outputView $ defaultTableView {
     tableSeries =
        resultByName "t" <>
        resultByName "a" <>
        resultByName "b" <>
        resultByName "c" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Time Series",
     timeSeriesLeftYSeries =
        resultByName "a" <>
        resultByName "b" <>
        resultByName "c" },
   outputView $ defaultXYChartView {
     xyChartTitle = "XYChart - 1",
     xyChartPlotTitle = "b=b(a), c=c(a)",
     xyChartXSeries =
       resultByName "a",
     xyChartLeftYSeries =
       resultByName "b",
     xyChartRightYSeries =
       resultByName "c" },
   outputView $ defaultXYChartView {
     xyChartTitle = "XYChart - 2",
     xyChartPlotTitle = "a=a(b), c=c(b)",
     xyChartXSeries =
       resultByName "b",
     xyChartRightYSeries =
       resultByName "a" <>
       resultByName "c" },
   outputView $ defaultXYChartView {
     xyChartTitle = "XYChart - 3",
     xyChartPlotTitle = "a=a(c), b=b(c)",
     xyChartXSeries =
       resultByName "c",
     xyChartLeftYSeries =
       resultByName "b",
     xyChartRightYSeries =
       resultByName "a" } ]
