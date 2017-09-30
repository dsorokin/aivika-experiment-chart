
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
    experimentTitle = "A Truck Hauling Situation",
    experimentDescription = description }

description =
  "The system to be modeled in this example consists of one bulldozer, four trucks, \
  \ and two man-machine loaders. The bulldozer stockpiles material for the loaders. \
  \ Two piles of material must be stocked prior to the initiation of any load operation. \
  \ The time for the bulldozer to stockpile material is Erlang distributed and consists \
  \ of the sum of two exponential variables each with a men of 4. (This corresponds to \
  \ an Erlang variable with a mean of 8 and a variance of 32.) In addition to this \
  \ material, a loader and an unloaded truck must be available before the loading \
  \ operations can begin. Loading time is exponentially distributed with a mean time of \
  \ 14 minutes for server 1 and 12 minutes for server 2. \
  \ \
  \ After a truck is loaded, it is hauled, then dumped and must be returned before \
  \ the truck is available for further loading. Hauling time is normally distributed. \
  \ When loaded, the average hauling time is 22 minutes. When unloaded, the average \
  \ time is 18 minutes. In both cases, the standard deviation is 3 minutes. Dumping \
  \ time is uniformly distributed between 2 and 8 minutes. Following a loading \
  \ operation, the loaded must rest for a 5 minute period before he is available \
  \ to begin loading again. The system is to be analyzed for 8 hours and all operations \
  \ in progress at the end of 8 hours should be completed before terminating \
  \ the operations for a run."

loadQueue           = T.Queue $ resultByName "loadQueue"
loadQueueCount      = T.queueCount loadQueue
loadQueueCountStats = T.tr $ T.queueCountStats loadQueue
loadWaitTime        = T.tr $ T.queueWaitTime loadQueue

truckQueue           = T.Queue $ resultByName "truckQueue"
truckQueueCount      = T.queueCount truckQueue
truckQueueCountStats = T.tr $ T.queueCountStats truckQueue
truckWaitTime        = T.tr $ T.queueWaitTime truckQueue

loaderQueue           = T.Queue $ resultByName "loaderQueue"
loaderQueueCount      = T.queueCount loaderQueue
loaderQueueCountStats = T.tr $ T.queueCountStats loaderQueue
loaderWaitTime        = T.tr $ T.queueWaitTime loaderQueue

loaderOps               = T.Operation $ resultByName "loaderOps"
loaderUtilisationTime   = T.tr $ T.operationUtilisationTime loaderOps
loaderUtilisationFactor = T.operationUtilisationFactor loaderOps

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
