
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 3600.0,
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
    experimentTitle = "Single-Lane Traffic Analysis" }

waitTime       = resultByName "waitTime"
greenLightTime = resultByName "greenLightTime"

start                      = T.Resource $ resultByName "start"
startQueueCount            = T.resourceQueueCount start
startQueueCountStats       = T.tr $ T.resourceQueueCountStats start
startWaitTime              = T.tr $ T.resourceWaitTime start
startCountStats            = T.tr $ T.resourceCountStats start
startUtilisationCountStats = T.tr $ T.resourceUtilisationCountStats start

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "Green Light Time (Initial Conditions)",
     finalStatsSeries = greenLightTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Average Waiting Time",
     finalStatsSeries = waitTime },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Average Waiting Time Chart",
     deviationChartWidth = 1000,
     deviationChartRightYSeries = waitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Queue Count",
     finalStatsSeries = startQueueCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Resource Queue Count Chart",
     deviationChartWidth = 1000,
     deviationChartRightYSeries =
       startQueueCount <> startQueueCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Wait Time",
     finalStatsSeries = startWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Utilisation Summary",
     finalStatsSeries = startUtilisationCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Availability Summary",
     finalStatsSeries = startCountStats } ]
