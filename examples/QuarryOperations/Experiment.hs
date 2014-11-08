
module Experiment(experiment, generators) where

import Control.Category
import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
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
    experimentTitle = "Quarry Operations" }

shovelQueueSize :: ResultTransform
shovelQueueSize =
  (resultByName "shovelQueue" >>>
   resultById QueueCountId)

shovelQueueWaitTime :: ResultTransform
shovelQueueWaitTime =
  (resultByName "shovelQueue" >>>
   resultById QueueWaitTimeId)

shovelQueueRate :: ResultTransform
shovelQueueRate =
  (resultByName "shovelQueue" >>>
   resultById QueueRateId)

crusherQueueSize :: ResultTransform
crusherQueueSize =
  (resultByName "crusherQueue" >>>
   resultById QueueCountId)

crusherQueueWaitTime :: ResultTransform
crusherQueueWaitTime =
  (resultByName "crusherQueue" >>>
   resultById QueueWaitTimeId)

crusherQueueRate :: ResultTransform
crusherQueueRate =
  (resultByName "crusherQueue" >>>
   resultById QueueRateId)

shovelUtilisationFactor :: ResultTransform
shovelUtilisationFactor =
  (resultByName "shovelActvty" >>>
   resultById ActivityUtilisationFactorId)

crusherUtilisationFactor :: ResultTransform
crusherUtilisationFactor =
  (resultByName "crusherActvty" >>>
   resultById ActivityUtilisationFactorId)

subgenerators1 :: ChartRendering r => String -> ResultTransform -> [WebPageGenerator r]
subgenerators1 title series =
  [outputView $ defaultDeviationChartView {
     deviationChartTitle = title ++ " (chart)",
     deviationChartRightYSeries = series },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = title ++ " (histogram)",
     finalHistogramSeries = series },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = title ++ " (statistics)",
     finalStatsSeries = series } ]

subgenerators2 :: ChartRendering r => String -> ResultTransform -> [WebPageGenerator r]
subgenerators2 title series =
  [outputView $ defaultDeviationChartView {
     deviationChartTitle = title ++ " (chart)",
     deviationChartRightYSeries = series },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = title ++ " (statistics)",
     finalStatsSeries = series } ]

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView] <>
  subgenerators1 "The shovel queue size" shovelQueueSize <>
  subgenerators2 "The shovel queue wait time" shovelQueueWaitTime <>
  subgenerators1 "The shovel queue rate" shovelQueueRate <>
  subgenerators1 "The crusher queue size" crusherQueueSize <>
  subgenerators2 "The crusher queue wait time" crusherQueueWaitTime <>
  subgenerators1 "The crusher queue rate" crusherQueueRate <>
  subgenerators1 "The shovel utilisation factor" shovelUtilisationFactor <>
  subgenerators1 "The crusher utilisation factor" crusherUtilisationFactor
