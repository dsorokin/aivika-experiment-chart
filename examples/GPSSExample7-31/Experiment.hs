
{-# LANGUAGE FlexibleContexts #-}

module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T
import qualified Simulation.Aivika.GPSS.Results.Transform as GpssT

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 20000.0,
                spcDT = 10.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 100,
    -- experimentRunCount = 10,
    experimentTitle = "The GPSS module 7.31" }

worker = GpssT.Facility $ resultByName "workr"
workerUtilCount = GpssT.facilityUtilisationCount worker
workerUtilCountStats = T.tr $ GpssT.facilityUtilisationCountStats worker
workerHoldingTime = T.tr $ GpssT.facilityHoldingTime worker

tatym = resultByName "stats"

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
   outputView $ statsView "Worker Utilisation (WORKR)" $
     workerUtilCount <>
     workerUtilCountStats,
   outputView $ chartView "Worker Utilisation (WORKR)" $
     workerUtilCount <> 
     workerUtilCountStats,
   outputView $ statsView "Worker Holding Time (WORKR)"
     workerHoldingTime,
   outputView $ chartView "Worker Holding Time (WORKR)"
     workerHoldingTime,
   outputView $ statsView "Processing Time (TATYM)" $
     tatym,
   outputView $ chartView "Processing Time (TATYM)" $
     tatym]
