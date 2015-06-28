
module Experiment (experiment, generators) where

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0, 
                spcStopTime = 25, 
                spcDT = 0.01,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Bouncing Ball",
    experimentDescription = "Simulation of a Bouncing Ball as described in " ++
                            "the corresponded MATLAB & Simulink example" }

t = resultByName "t"
x = resultByName "x"
v = resultByName "v"

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultTableView {
     tableSeries = t <> x <> v },
   outputView $ defaultTimeSeriesView {
     timeSeriesDescription = "The chart shows the position of the ball",
     timeSeriesTitle = "Position",
     timeSeriesLeftYSeries = x },
   outputView $ defaultTimeSeriesView {
     timeSeriesDescription = "The chart shows the velocity of the ball",
     timeSeriesTitle = "Velocity",
     timeSeriesLeftYSeries = v } ]
