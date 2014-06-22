
module Experiment (experiment) where

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 8.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: ChartRenderer r => Experiment r
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 20,
    experimentDescription = "This is the famous Bass Diffusion model solved with help of the agent-based modelling.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       outputView $ defaultDeviationChartView {
         deviationChartSeries = [Left "potentialAdopters", 
                                 Left "adopters"] },
       outputView $ defaultTimeSeriesView {
         timeSeries = [Left "potentialAdopters", 
                       Left "adopters"] } ]
    }
