
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

totalCustomerCount :: ResultTransform
totalCustomerCount = resultByName "totalCustomerCount"

immedSalesCount :: ResultTransform
immedSalesCount = resultByName "immedSalesCount"

backorderWaitTime :: ResultTransform
backorderWaitTime = 
  resultByName "backorderQueue" >>> 
  resultById QueueWaitTimeId

backorderQueueSize :: ResultTransform
backorderQueueSize = 
  resultByName "backorderQueue" >>> 
  resultById QueueCountStatsId >>> 
  expandResults >>> 
  resultById TimingStatsMeanId

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle  = "Total Customer and Immediate Sales Counts",
     finalStatsSeries = totalCustomerCount <> immedSalesCount },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Backorder Wait Time (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = backorderWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Backorder Wait Time (statistics)",
     finalStatsSeries = backorderWaitTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Backorder Queue Size (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = backorderQueueSize },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = "The Backorder Queue Size (histogram)",
     finalHistogramWidth = 1000,
     finalHistogramSeries = backorderQueueSize },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Backorder Queue Size (statistics)",
     finalStatsSeries = backorderQueueSize } ]
