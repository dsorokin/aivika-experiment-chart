
module Experiment (experiment, generators) where

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 8.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 20,
    experimentDescription = "This is the famous Bass Diffusion model solved with help of the agent-based modelling." }

generators :: WebPageCharting r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultDeviationChartView {
     deviationChartLeftYSeries = 
        resultByName "potentialAdopters" <>
        resultByName "adopters" },
    outputView $ defaultTimeSeriesView {
      timeSeriesLeftYSeries =
         resultByName "potentialAdopters" <>
         resultByName "adopters" } ]
