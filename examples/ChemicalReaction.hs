
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.SystemDynamics
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.LastValueView
import Simulation.Aivika.Experiment.TableView
import Simulation.Aivika.Experiment.TimeSeriesView
import Simulation.Aivika.Experiment.XYChartView
import Simulation.Aivika.Experiment.ExperimentSpecsView

specs = Specs { spcStartTime = 0, 
                spcStopTime = 13, 
                spcDT = 0.01,
                spcMethod = RungeKutta4 }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       outputView $ defaultLastValueView {
         lastValueSeries = ["t", "a", "b", "c"] },
       outputView $ defaultTableView {
         tableSeries = ["t", "a", "b", "c"] }, 
       outputView $ defaultTimeSeriesView {
         timeSeries = [Left "a", Left "b", Left "c"] },
       -- outputView $ defaultTimeSeriesView {
       --   timeSeriesPlotTitle = "Variables a, b and c for t <= 5 or t >= 7",
       --   timeSeries = [Left "a", Left "b", Left "c"],
       --   timeSeriesPredicate =
       --     do t <- time
       --        return (t <= 5 || t >= 7) },
       outputView $ defaultXYChartView {
         xyChartXSeries = Just "a",
         xyChartYSeries = [Left "b", Right "c"] },
       outputView $ defaultXYChartView {
         xyChartXSeries = Just "b",
         xyChartYSeries = [Right "a", Right "c"] },
       outputView $ defaultXYChartView {
         xyChartXSeries = Just "c",
         xyChartYSeries = [Right "a", Left "b"] } ] }
       -- outputView $ defaultXYChartView {
       --   xyChartPlotTitle = "Functions a=a(c) and b=b(c) for t <= 2 or t >= 3",
       --   xyChartXSeries = Just "c",
       --   xyChartYSeries = [Right "a", Left "b"],
       --   xyChartPredicate = 
       --     do t <- time
       --        return (t <= 2 || t >= 3) } ] }

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
     experimentDataInStartTime queue
       [("t", seriesEntity "time" time),
        ("a", seriesEntity "a" a),
        ("b", seriesEntity "b" b),
        ("c", seriesEntity "c" c)]

main = runExperiment experiment model
