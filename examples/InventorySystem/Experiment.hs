
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

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

radio :: ResultTransform
radio = resultByName "radio"

invPos :: ResultTransform
invPos = 
  resultByName "invPos" >>>
  resultById TimingCounterValueId

invPosStats :: ResultTransform
invPosStats =
  resultByName "invPos" >>>
  resultById TimingCounterStatsId

tbLostSales :: ResultTransform
tbLostSales = resultByName "tbLostSales"

tbLostSalesCount :: ResultTransform
tbLostSalesCount =
  tbLostSales >>>
  resultById SamplingStatsCountId

safetyStock :: ResultTransform
safetyStock = resultByName "safetyStock"

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle  = "Inventory Position and Time Between Lost Sales",
     finalStatsSeries = invPosStats <> tbLostSales },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "Radios, Inventory Position and Safety Stock",
     -- deviationChartWidth = 1000,
     deviationChartRightYSeries = radio <> invPosStats <> safetyStock },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Safety Stock",
     finalStatsSeries = safetyStock } ]
