
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
                spcStopTime = 500000.0,
                spcDT = 1000.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 500,
    -- experimentRunCount = 10,
    experimentTitle = "The GPSS module 7.31" }

line = GpssT.Queue $ resultByName "line"
lineContent = GpssT.queueContent line
lineContentStats = T.tr $ GpssT.queueContentStats line
lineWaitTime = T.tr $ GpssT.queueWaitTime line
lineNonZeroEntryWaitTime = T.tr $ GpssT.queueNonZeroEntryWaitTime line

prof = GpssT.Facility $ resultByName "prof"
profUtilCount = GpssT.facilityUtilisationCount prof
profUtilCountStats = T.tr $ GpssT.facilityUtilisationCountStats prof
profHoldingTime = T.tr $ GpssT.facilityHoldingTime prof

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
  
histogramView title series = 
  defaultFinalHistogramView {
    finalHistogramTitle = title,
    finalHistogramSeries = series
  }

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ statsView "PROF Utilisation" $
     profUtilCount <>
     profUtilCountStats,
   outputView $ chartView "PROF Utilisation" $
     profUtilCount <> 
     profUtilCountStats,
   outputView $ statsView "PROF Holding Time"
     profHoldingTime,
   outputView $ chartView "PROF Holding Time"
     profHoldingTime,
   outputView $ statsView "LINE Content" $
     lineContent <>
     lineContentStats,
   outputView $ chartView "LINE Content" $
     lineContent <>
     lineContentStats,
   outputView $ histogramView "LINE Content"
     lineContent,
   outputView $ statsView "LINE Wait Time" $
     lineWaitTime <>
     lineNonZeroEntryWaitTime,
   outputView $ chartView "LINE Wait Time" $
     lineWaitTime <>
     lineNonZeroEntryWaitTime]
