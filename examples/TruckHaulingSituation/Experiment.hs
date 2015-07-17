
{-# LANGUAGE FlexibleContexts #-}

module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
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
    experimentTitle = "A Truck Hauling Situation" }

loadQueue           = T.Queue $ resultByName "loadQueue"
loadQueueCount      = T.tr $ T.queueCount loadQueue
loadQueueCountStats = T.tr $ T.queueCountStats loadQueue
loadWaitTime        = T.tr $ T.queueWaitTime loadQueue

truckQueue           = T.Queue $ resultByName "truckQueue"
truckQueueCount      = T.tr $ T.queueCount truckQueue
truckQueueCountStats = T.tr $ T.queueCountStats truckQueue
truckWaitTime        = T.tr $ T.queueWaitTime truckQueue

loaderQueue           = T.Queue $ resultByName "loaderQueue"
loaderQueueCount      = T.tr $ T.queueCount loaderQueue
loaderQueueCountStats = T.tr $ T.queueCountStats loaderQueue
loaderWaitTime        = T.tr $ T.queueWaitTime loaderQueue

loaderOps               = T.Operation $ resultByName "loaderOps"
loaderUtilisationTime   = T.tr $ T.operationUtilisationTime loaderOps
loaderUtilisationFactor = T.tr $ T.operationUtilisationFactor loaderOps

statsView title series =
  defaultFinalStatsView {
    finalStatsTitle = title,
    finalStatsSeries = series 
  }

chartView title series = 
  defaultDeviationChartView {
    deviationChartTitle = title,
    deviationChartRightYSeries = series
  }

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ statsView "Queue Length" $
     loadQueueCountStats <>
     truckQueueCountStats <>
     loaderQueueCountStats,
   outputView $ chartView "Queue Load" $
     loadQueueCount <> loadQueueCountStats,
   outputView $ chartView "Queue Trucks" $
     truckQueueCount <> truckQueueCountStats,
   outputView $ chartView "Queue Loader" $
     loaderQueueCount <> loaderQueueCountStats,
   outputView $ statsView "Queue Waiting Time" $
     loadWaitTime <>
     truckWaitTime <>
     loaderWaitTime,
   outputView $ chartView "Loader Utilisation Chart"
     loaderUtilisationFactor,
   outputView $ statsView "Loader Utilisation Summary" $
     loaderUtilisationFactor <>
     loaderUtilisationTime]
