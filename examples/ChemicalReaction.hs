
{-# LANGUAGE RecursiveDo #-}

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
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
                            "the 5-minute tutorial of Berkeley-Madonna",
    experimentGenerators = 
      [outputView defaultExperimentSpecsView,
       outputView $ defaultLastValueView {
         lastValueSeries = ["t", "a", "b", "c"] },
       outputView $ defaultTableView {
         tableSeries = ["t", "a", "b", "c"] }, 
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time Series",
         timeSeries = [Left "a", Left "b", Left "c"] },
       -- outputView $ defaultTimeSeriesView {
       --   timeSeriesPlotTitle = "Variables a, b and c for t <= 5 or t >= 7",
       --   timeSeries = [Left "a", Left "b", Left "c"],
       --   timeSeriesPredicate =
       --     do t <- time
       --        return (t <= 5 || t >= 7) },
       outputView $ defaultXYChartView {
         xyChartTitle = "XYChart - 1",
         xyChartPlotTitle = "b=b(a), c=c(a)",
         xyChartXSeries = Just "a",
         xyChartYSeries = [Left "b", Right "c"] },
       outputView $ defaultXYChartView {
         xyChartTitle = "XYChart - 2",
         xyChartPlotTitle = "a=a(b), c=c(b)",
         xyChartXSeries = Just "b",
         xyChartYSeries = [Right "a", Right "c"] },
       outputView $ defaultXYChartView {
         xyChartTitle = "XYChart - 3",
         xyChartPlotTitle = "a=a(c), b=b(c)",
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
  mdo a <- integ (- ka * a) 100
      b <- integ (ka * a - kb * b) 0
      c <- integ (kb * b) 0
      let ka = 1
          kb = 1
      experimentDataInStartTime
        [("t", seriesEntity "time" time),
         ("a", seriesEntity "a" a),
         ("b", seriesEntity "b" b),
         ("c", seriesEntity "c" c)]

main = runExperiment experiment model
