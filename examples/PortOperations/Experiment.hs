
module Experiment (experiment, generators) where

import Data.Monoid

import Control.Arrow

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

import qualified Simulation.Aivika.Results.Transform as T

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

berth                 = T.Resource $ resultByName "berth"
berthCountStats       = T.tr $ T.resourceCountStats berth
berthUtilisationCount = T.tr $ T.resourceUtilisationCountStats berth
berthQueueCount       = T.resourceQueueCount berth
berthQueueCountStats  = T.tr $ T.resourceQueueCountStats berth
berthWaitTime         = T.tr $ T.resourceWaitTime berth

tug                 = T.Resource $ resultByName "tug"
tugCountStats       = T.tr $ T.resourceCountStats tug
tugUtilisationCount = T.tr $ T.resourceUtilisationCountStats tug
tugQueueCount       = T.resourceQueueCount tug
tugQueueCountStats  = T.tr $ T.resourceQueueCountStats tug
tugWaitTime         = T.tr $ T.resourceWaitTime tug

generators :: ChartRendering r => [WebPageGenerator r]
generators =
  [outputView defaultExperimentSpecsView,
   outputView defaultInfoView,
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Port Time Summary",
     finalStatsSeries = portTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Queue Length",
     finalStatsSeries = berthQueueCountStats <> tugQueueCountStats },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Wait Time",
     finalStatsSeries = berthWaitTime <> tugWaitTime },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Utilisation Summary",
     finalStatsSeries = berthUtilisationCount <> tugUtilisationCount },
   outputView $ defaultFinalStatsView {
     finalStatsTitle = "The Resource Availability Summary",
     finalStatsSeries = berthCountStats <> tugCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Berth Resource Queue Length",
     deviationChartRightYSeries = berthQueueCount <> berthQueueCountStats },
   outputView $ defaultDeviationChartView {
     deviationChartTitle = "The Tug Resource Queue Length",
     deviationChartRightYSeries = tugQueueCount <> tugQueueCountStats }]
