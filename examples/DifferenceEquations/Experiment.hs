
module Experiment (experiment, generators) where

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0, 
                spcStopTime = 10000, 
                spcDT = 1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Difference Equations",
    experimentDescription = "Difference Equations as described in " ++
                            "the corresponded tutorial of Berkeley-Madonna " ++
                            "with small modification for calculating std." }

t     = resultByName "t"
x     = resultByName "x"
sumX  = resultByName "sumX"
sumX2 = resultByName "sumX2"
avg   = resultByName "avg"
std   = resultByName "std"

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultTableView {
     tableSeries =
        t <> x <> sumX <> sumX2 <> avg <> std },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Time Series",
     timeSeriesLeftYSeries = x <> avg },
   outputView $ defaultTimingStatsView {
     timingStatsSeries = x },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Sums",
     timeSeriesLeftYSeries = sumX,
     timeSeriesRightYSeries = sumX2 },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Standard Deviation",
     timeSeriesLeftYSeries = std } ]
