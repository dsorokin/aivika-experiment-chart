
module Experiment (monteCarloExperiment, singleExperiment,
                   monteCarloGenerators, singleGenerators) where

import Control.Monad

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import Model

-- the simulation specs
specs = Specs 0 5 0.015625 RungeKutta4 SimpleGenerator

-- | The experiment for the Monte-Carlo simulation.
monteCarloExperiment :: Experiment
monteCarloExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    experimentTitle = "Financial Model (the Monte-Carlo simulation)",
    experimentDescription = "Financial Model (the Monte-Carlo simulation) as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk." }

netIncome = resultByName netIncomeName
npvIncome = resultByName npvIncomeName
  
netCashFlow = resultByName netCashFlowName
npvCashFlow = resultByName npvCashFlowName

monteCarloGenerators :: ChartRendering r => [WebPageGenerator r]
monteCarloGenerators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Chart 1",
     deviationChartPlotTitle = "The deviation chart for Net Income and Cash Flow",
     deviationChartLeftYSeries = netIncome <> netCashFlow },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Chart 2",
     deviationChartPlotTitle = "The deviation chart for Net Present Value of Income and Cash Flow",
     deviationChartLeftYSeries = npvIncome <> npvCashFlow },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Histogram 1",
     finalHistogramPlotTitle = "Histogram for Net Income and Cash Flow",
     finalHistogramSeries = netIncome <> netCashFlow },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "Histogram 2",
     finalHistogramPlotTitle = "Histogram for Net Present Value of Income and Cash Flow",
     finalHistogramSeries = npvIncome <> npvCashFlow },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Summary 1",
     finalStatsSeries = netIncome <> netCashFlow },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Summary 2",
     finalStatsSeries = npvIncome <> npvCashFlow } ]
  
-- | The experiment with single simulation run.
singleExperiment :: Experiment
singleExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentTitle = "Financial Model",
    experimentDescription = "Financial Model as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk." }

singleGenerators :: ChartRendering r => [WebPageGenerator r]
singleGenerators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Time Series 1",
     timeSeriesPlotTitle = "Time series of Net Income and Cash Flow",
     timeSeriesLeftYSeries = netIncome <> netCashFlow },
   outputView $ defaultTimeSeriesView {
     timeSeriesTitle = "Time Series 2",
     timeSeriesPlotTitle = "Time series of Net Present Value for Income and Cash Flow",
     timeSeriesLeftYSeries = npvIncome <> npvCashFlow },
   outputView $ defaultTableView {
     tableTitle = "Table",
     tableSeries = netIncome <> netCashFlow <> npvIncome <> npvCashFlow } ]
