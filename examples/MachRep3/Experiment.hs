
module Experiment (experiment) where

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

experiment :: ChartRenderer r => Experiment r
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 200,
    experimentDescription = "Experiment Description",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       outputView $ defaultDeviationChartView {
         deviationChartSeries = [Left "t", Right "x"] },
       outputView $ defaultFinalXYChartView {
         finalXYChartPlotTitle = "The proportion up time", 
         finalXYChartXSeries = Just "n",
         finalXYChartYSeries = [Right "x"] }, 
       outputView $ defaultFinalXYChartView {
         finalXYChartPlotTitle = "The proportion up time for simulation runs < 50 and > 100", 
         finalXYChartXSeries = Just "n",
         finalXYChartYSeries = [Right "x"], 
         finalXYChartPredicate =
           do i <- liftParameter simulationIndex
              return $ (i < 50) || (i > 100) },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Default)",
         finalHistogramSeries = ["x", "x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Default)",
         finalHistogramSeries = ["t", "t"] },
       outputView $ defaultFinalStatsView {
         finalStatsSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Default)",
         finalHistogramSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Bin Size = 0.001)",
         finalHistogramBuild  = histogramBinSize 0.001,
         finalHistogramSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Bin Num = 10)",
         finalHistogramBuild  = histogramNumBins 10,
         finalHistogramSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Sturges)",
         finalHistogramBuild  = histogram binSturges,
         finalHistogramSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Doane)",
         finalHistogramBuild  = histogram binDoane,
         finalHistogramSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Sqrt)",
         finalHistogramBuild  = histogram binSqrt,
         finalHistogramSeries = ["x"] },
       outputView $ defaultFinalHistogramView {
         finalHistogramPlotTitle  = "Final Histogram (Scott)",
         finalHistogramBuild  = histogram binScott,
         finalHistogramSeries = ["x"] } ] }
