
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 312.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    -- experimentRunCount = 10,
    experimentTitle = "Inventory System with Lost Sales and Backorders" }

radio = T.Resource $ resultByName "radio"
radioCount      = T.resourceCount radio
radioCountStats = T.tr $ T.resourceCountStats radio

invPos = T.TimingCounter $ resultByName "invPos"
invPosValue = T.timingCounterValue invPos
invPosStats = T.tr $ T.timingCounterStats invPos

tbLostSales = resultByName "tbLostSales"
tbLostSalesCount = 
  T.samplingStatsCount $
  T.SamplingStats tbLostSales

safetyStock :: ResultTransform
safetyStock = resultByName "safetyStock"

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle  = "Inventory Position and Time Between Lost Sales",
     finalStatsSeries = invPosStats <> tbLostSales },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Radios and Inventory Position",
     -- deviationChartWidth = 1000,
     deviationChartRightYSeries = 
       radioCount <> invPosValue },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Radios",
     -- deviationChartWidth = 1000,
     deviationChartRightYSeries = 
       radioCount <> radioCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Inventory Position",
     -- deviationChartWidth = 1000,
     deviationChartRightYSeries = 
       invPosValue <> invPosStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Safety Stock",
     -- deviationChartWidth = 1000,
     deviationChartRightYSeries = 
       safetyStock },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Safety Stock",
     finalStatsSeries = safetyStock } ]
