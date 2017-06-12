
module Experiment(experiment, generators) where

import Control.Category
import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

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

shovelQueue  = T.Queue $ resultByName "shovelQueue"
crusherQueue = T.Queue $ resultByName "crusherQueue"

shovelActvty  = T.Activity $ resultByName "shovelActvty"
crusherActvty = T.Activity $ resultByName "crusherActvty"

shovelQueueCount      = T.queueCount shovelQueue
shovelQueueCountStats = T.tr $ T.queueCountStats shovelQueue
shovelQueueWaitTime   = T.tr $ T.queueWaitTime shovelQueue
shovelQueueRate       = T.queueRate shovelQueue

crusherQueueCount      = T.queueCount crusherQueue
crusherQueueCountStats = T.tr $ T.queueCountStats crusherQueue
crusherQueueWaitTime   = T.tr $ T.queueWaitTime crusherQueue
crusherQueueRate       = T.queueRate crusherQueue

shovelUtilisationFactor  = T.activityUtilisationFactor shovelActvty
crusherUtilisationFactor = T.activityUtilisationFactor crusherActvty

subgenerators1 :: ChartRendering r => String -> ResultTransform -> [WebPageGenerator r]
subgenerators1 title series =
  [outputView $ defaultDeviationChartView {
     deviationChartTitle = title ++ " (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = series },
   outputView $ defaultFinalHistogramView {
     finalHistogramTitle = title ++ " (histogram)",
     finalHistogramWidth = 1000,
     finalHistogramSeries = series },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = title ++ " (statistics)",
     finalStatsSeries = series } ]

subgenerators2 :: ChartRendering r => String -> ResultTransform -> [WebPageGenerator r]
subgenerators2 title series =
  [outputView $ defaultDeviationChartView {
     deviationChartTitle = title ++ " (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = series },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = title ++ " (statistics)",
     finalStatsSeries = series } ]

subgenerators3 :: ChartRendering r => String -> ResultTransform -> ResultTransform -> [WebPageGenerator r]
subgenerators3 title series1 series2 =
  [outputView $ defaultDeviationChartView {
     deviationChartTitle = title ++ " (chart)",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = series1 <> series2 },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = title ++ " (statistics)",
     finalStatsSeries = series2 } ]

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView] <>
  subgenerators3 "The shovel queue size" shovelQueueCount shovelQueueCountStats <>
  subgenerators2 "The shovel queue wait time" shovelQueueWaitTime <>
  subgenerators1 "The shovel queue rate" shovelQueueRate <>
  subgenerators3 "The crusher queue size" crusherQueueCount crusherQueueCountStats <>
  subgenerators2 "The crusher queue wait time" crusherQueueWaitTime <>
  subgenerators1 "The crusher queue rate" crusherQueueRate <>
  subgenerators1 "The shovel utilisation factor" shovelUtilisationFactor <>
  subgenerators1 "The crusher utilisation factor" crusherUtilisationFactor
