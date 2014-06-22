
{-# LANGUAGE RecursiveDo #-}

module Experiment (monteCarloExperiment, singleExperiment) where

import Control.Monad

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import Model

-- the simulation specs
specs = Specs 0 5 0.015625 RungeKutta4 SimpleGenerator

-- | The experiment for the Monte-Carlo simulation.
monteCarloExperiment :: ChartRenderer r => Experiment r
monteCarloExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    experimentTitle = "Financial Model (the Monte-Carlo simulation)",
    experimentDescription = "Financial Model (the Monte-Carlo simulation) as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The deviation chart for Net Income and Cash Flow",
         deviationChartSeries = [Left netIncomeName, 
                                 Left netCashFlowName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The deviation chart for Net Present Value of Income and Cash Flow",
         deviationChartSeries = [Left npvIncomeName, 
                                 Left npvCashFlowName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Histogram for Net Income and Cash Flow",
         finalHistogramSeries = [netIncomeName, netCashFlowName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Histogram for Net Present Value of Income and Cash Flow",
         finalHistogramSeries = [npvIncomeName, npvCashFlowName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Summary for Net Income and Cash Flow",
         finalStatsSeries = [netIncomeName, netCashFlowName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Summary for Net Present Value of Income and Cash Flow",
         finalStatsSeries = [npvIncomeName, npvCashFlowName] } ] }

-- | The experiment with single simulation run.
singleExperiment :: ChartRenderer r => Experiment r
singleExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentTitle = "Financial Model",
    experimentDescription = "Financial Model as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time series of Net Income and Cash Flow",
         timeSeries = [Left netIncomeName, 
                       Left netCashFlowName] },
       
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time series of Net Present Value for Income and Cash Flow",
         timeSeries = [Left npvIncomeName, 
                       Left npvCashFlowName] },

       outputView $ defaultTableView {
         tableTitle = "Table",
         tableSeries = [netIncomeName, netCashFlowName,
                        npvIncomeName, npvCashFlowName] } ] }
