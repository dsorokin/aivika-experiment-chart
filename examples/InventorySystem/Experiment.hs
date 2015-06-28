
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

radio :: ResultTransform
radio =
  T.tr $
  T.resourceCount $
  T.Resource $ resultByName "radio"

invPos :: ResultTransform
invPos =
  T.tr $
  T.timingCounterValue $
  T.TimingCounter $ resultByName "invPos"

invPosStats :: ResultTransform
invPosStats =
  T.tr $
  T.timingCounterStats $
  T.TimingCounter $ resultByName "invPos"

tbLostSales :: ResultTransform
tbLostSales = resultByName "tbLostSales"

tbLostSalesCount :: ResultTransform
tbLostSalesCount =
  T.tr $
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
     deviationChartTitle = "Radios, Inventory Position and Safety Stock",
     -- deviationChartWidth = 1000,
     deviationChartRightYSeries = radio <> invPosStats <> safetyStock },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Safety Stock",
     finalStatsSeries = safetyStock } ]
