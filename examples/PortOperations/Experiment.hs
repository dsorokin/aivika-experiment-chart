
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 8760.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 500,
    -- experimentRunCount = 10,
    experimentTitle = "Port Operations" }

portTime = resultByName "portTime"

berth = resultByName "berth"
berthQueueCount = berth >>> resultById ResourceQueueCountStatsId
berthWaitTime = berth >>> resultById ResourceWaitTimeId
berthCount = berth >>> resultById ResourceCountStatsId
berthUtilisationCount = berth >>> resultById ResourceUtilisationCountStatsId

tug = resultByName "tug"
tugCount = tug >>> resultById ResourceCountStatsId
tugQueueCount = tug >>> resultById ResourceQueueCountStatsId
tugWaitTime = tug >>> resultById ResourceWaitTimeId
tugUtilisationCount = tug >>> resultById ResourceUtilisationCountStatsId

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Port Time Summary",
     finalStatsSeries = portTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Queue Length",
     finalStatsSeries = berthQueueCount <> tugQueueCount },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Wait Time",
     finalStatsSeries = berthWaitTime <> tugWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Utilisation Summary",
     finalStatsSeries = berthUtilisationCount <> tugUtilisationCount },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Availability Summary",
     finalStatsSeries = berthCount <> tugCount } ]
