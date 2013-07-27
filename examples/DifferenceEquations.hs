
{-# LANGUAGE RecursiveDo #-}

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Random
import Simulation.Aivika.SystemDynamics

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.TableView
import Simulation.Aivika.Experiment.TimeSeriesView
import Simulation.Aivika.Experiment.ExperimentSpecsView
import Simulation.Aivika.Experiment.TimingStatsView

specs = Specs { spcStartTime = 0, 
                spcStopTime = 10000, 
                spcDT = 1,
                spcMethod = RungeKutta4 }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Difference Equations",
    experimentDescription = "Difference Equations as described in " ++
                            "the corresponded tutorial of Berkeley-Madonna " ++
                            "with small modification for calculating std.",
    experimentGenerators = 
      [outputView defaultExperimentSpecsView,
       outputView $ defaultTableView {
         tableSeries = ["t", "x", "sumX", "sumX2", "avg", "std"] }, 
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time Series",
         timeSeries = [Left "x", Left "avg"] },
       outputView $ defaultTimingStatsView {
         timingStatsSeries = ["x"] },
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Sums",
         timeSeries = [Left "sumX", Right "sumX2"] },
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Standard Deviation",
         timeSeries = [Left "std"] } ] }

model :: Simulation ExperimentData
model =
  mdo x <- newNormalDynamics 3 0.8
      sumX <- sumDynamics x 0
      sumX2 <- sumDynamics (x * x) 0
      
      -- it would be much more efficient to say:
      --   let n = fmap fromIntegral integIteration
      n <- sumDynamics 1 0

      let avg = ifDynamics (n .>. 0) (sumX / n) 0
      let std = ifDynamics (n .>. 1) (sqrt ((sumX2 - sumX * avg) / (n - 1))) 0
      
      experimentDataInStartTime
        [("t", seriesEntity "time" time),
         ("n", seriesEntity "n" n),
         ("x", seriesEntity "x" x),
         ("sumX", seriesEntity "sumX" sumX),
         ("sumX2", seriesEntity "sumX2" sumX2),
         ("avg", seriesEntity "avg" avg),
         ("std", seriesEntity "std" std)]

main = runExperiment experiment model
