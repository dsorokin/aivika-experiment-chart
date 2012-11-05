
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.SystemDynamics
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.LastValueView
import Simulation.Aivika.Experiment.TableView
import Simulation.Aivika.Experiment.TimeSeriesView

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4 }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentDescription = "Experiment Description",
    experimentGenerators =
      [outputView $ defaultLastValueView {
          lastValueDescription = "Last Value description",
          lastValueSeries = ["t", "a", "b", "c"] },
       outputView $ defaultTableView {
         tableDescription = "Table description",
         tableSeries = ["t", "a", "b", "c"] }, 
       outputView $ defaultTimeSeriesView {
         timeSeries = [Left "a", Left "b", Left "c"] } ] }

model :: Simulation ExperimentData
model =
  do queue  <- newQueue
     integA <- newInteg 100
     integB <- newInteg 0
     integC <- newInteg 0
     let a = integValue integA
         b = integValue integB
         c = integValue integC
     let ka = 1
         kb = 1
     integDiff integA (- ka * a)
     integDiff integB (ka * a - kb * b)
     integDiff integC (kb * b)
     experimentDataInStartTime queue $
       [("t", seriesEntity "time" time),
        ("a", seriesEntity "a" a),
        ("b", seriesEntity "b" b),
        ("c", seriesEntity "c" c)]

main = runExperiment experiment model
