
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

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultTableView {
     tableSeries =
        mconcat $ map resultByName $
        ["t", "x", "sumX", "sumX2", "avg", "std"] }, 
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Time Series",
     timeSeriesLeftYSeries =
       resultByName "x" <>
       resultByName "avg" },
   outputView $ defaultTimingStatsView {
     timingStatsSeries =
        resultByName "x" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Sums",
     timeSeriesLeftYSeries =
       resultByName "sumX",
     timeSeriesRightYSeries =
       resultByName "sumX2" },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Standard Deviation",
     timeSeriesLeftYSeries =
       resultByName "std" } ]
