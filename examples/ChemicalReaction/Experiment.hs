
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

t = resultByName "t"
a = resultByName "a"
b = resultByName "b"
c = resultByName "c"

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultLastValueView {
     lastValueSeries = t <> a <> b <> c },
   outputView $ defaultTableView {
     tableSeries = t <> a <> b <> c },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Time Series",
     timeSeriesLeftYSeries = a <> b <> c },
   outputView $ defaultXYChartView {
     xyChartTitle = "XYChart - 1",
     xyChartPlotTitle = "b=b(a), c=c(a)",
     xyChartXSeries = a,
     xyChartLeftYSeries = b,
     xyChartRightYSeries = c },
   outputView $ defaultXYChartView {
     xyChartTitle = "XYChart - 2",
     xyChartPlotTitle = "a=a(b), c=c(b)",
     xyChartXSeries = b,
     xyChartRightYSeries = a <> c },
   outputView $ defaultXYChartView {
     xyChartTitle = "XYChart - 3",
     xyChartPlotTitle = "a=a(c), b=b(c)",
     xyChartXSeries = c,
     xyChartLeftYSeries = b,
     xyChartRightYSeries = a } ]
