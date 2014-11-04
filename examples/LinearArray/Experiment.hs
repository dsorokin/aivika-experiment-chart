
module Experiment (experiment, generators) where

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0, 
                spcStopTime = 500, 
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Linear Array",
    experimentDescription = "Model Linear Array as described in " ++
                            "the examples included in Berkeley-Madonna." }

generators :: ChartRendering r => [WebPageGenerator r]
generators = 
  [outputView defaultExperimentSpecsView,
   outputView $ defaultTableView {
     tableSeries =
        resultByName "t" <>
        resultByName "m" <>
        resultByName "c" },
   outputView $ defaultTimeSeriesView {
     timeSeriesLeftYSeries =
        resultByName "m",
     timeSeriesWidth = 800,
     timeSeriesHeight = 800 },
   outputView $ defaultTimeSeriesView {
     timeSeriesRightYSeries =
        resultByName "c",
     timeSeriesWidth = 800,
     timeSeriesHeight = 800 },
   outputView $ defaultTimeSeriesView {
     timeSeriesLeftYSeries =
        resultByName "m",
     timeSeriesRightYSeries =
       resultByName "c",
     timeSeriesWidth = 800,
     timeSeriesHeight = 800 },
   outputView $ defaultXYChartView {
     xyChartXSeries =
        resultByName "t",
     xyChartLeftYSeries =
       resultByName "m",
     xyChartWidth = 800,
     xyChartHeight = 800 },
   outputView $ defaultXYChartView {
     xyChartXSeries =
        resultByName "t",
     xyChartRightYSeries =
       resultByName "c",
     xyChartWidth = 800,
     xyChartHeight = 800 },
   outputView $ defaultXYChartView {
     xyChartXSeries =
        resultByName "t",
     xyChartLeftYSeries =
       resultByName "m",
     xyChartRightYSeries =
       resultByName "c",
     xyChartWidth = 800,
     xyChartHeight = 800 } ]
