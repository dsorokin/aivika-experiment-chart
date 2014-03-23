
{-# LANGUAGE RecursiveDo, Arrows #-}

-- Example: Inspection and Adjustment Stations on a Production Line
-- 
-- This is a model of the workflow with a loop. Also there are two infinite queues.
--
-- It is described in different sources [1, 2]. So, this is chapter 8 of [2] and section 5.15 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

-- CAUTION:
--
-- This model is not yet fully tested and it may contain logical errors but it seems to be working,
-- although some results may differ slightly but it can be related to a great value of the deviation
-- for some variables as well as to a small number of samples in [1].
--
-- The results for the queue sizes in [2] seem doubtful for me, while my results for these queue sizes
-- are similar to [1] but I made 1000 runs versus 1 run in [1]. In comparison with [1] I see a difference
-- in the queue size for the adjustment station and it can be realized as there was a too small number of
-- samples (= 13) in [1], for the TV settings must fail when inspecting to be directed to the adjustor.
--
-- Also I have received more small values for the wait time in comparison with [1] but they have
-- a relatively great deviation, which may be acceptable (??), taking into account a small number of
-- samples used in [1].
--
-- At the same time, all my other results except for these queue sizes correspond to [2], where the author
-- launched 1000 simulation runs too.
--
-- Some new things that I have added the past summer (2013), i.e. Streams / Processors / Queues / Servers,
-- should be yet verified for other models but, as I wrote, they seem to be working.

import Prelude hiding (id, (.)) 

import Control.Monad
import Control.Monad.Trans
import Control.Arrow
import Control.Category (id, (.))

import Simulation.Aivika
import Simulation.Aivika.Queue.Infinite

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- the minimum delay of arriving the next TV set
minArrivalDelay = 3.5

-- the maximum delay of arriving the next TV set
maxArrivalDelay = 7.5

-- the minimum time to inspect the TV set
minInspectionTime = 6

-- the maximum time to inspect the TV set
maxInspectionTime = 12

-- the probability of passing the inspection phase
inspectionPassingProb = 0.85

-- how many are inspection stations?
inspectionStationCount = 2

-- the minimum time to adjust an improper TV set
minAdjustmentTime = 20

-- the maximum time to adjust an improper TV set
maxAdjustmentTime = 40

-- how many are adjustment stations?
adjustmentStationCount = 1

-- create an accumulator to gather the queue size statistics 
newQueueSizeAccumulator queue =
  newTimingStatsAccumulator $
  Signalable (queueCount queue) (queueCountChanged_ queue)

-- create an inspection station (server)
newInspectionStation =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minInspectionTime maxInspectionTime)
     passed <- 
       liftParameter $
       randomTrue inspectionPassingProb
     if passed
       then return $ Right a
       else return $ Left a 

-- create an adjustment station (server)
newAdjustmentStation =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minAdjustmentTime maxAdjustmentTime)
     return a
  
model :: Simulation ExperimentData
model = mdo
  -- to count the arrived TV sets for inspecting and adjusting
  inputArrivalTimer <- newArrivalTimer
  -- it will gather the statistics of the processing time
  outputArrivalTimer <- newArrivalTimer
  -- define a stream of input events
  let inputStream =
        randomUniformStream minArrivalDelay maxArrivalDelay 
  -- create a queue before the inspection stations
  inspectionQueue <- newFCFSQueue
  -- create a queue before the adjustment stations
  adjustmentQueue <- newFCFSQueue
  -- the inspection stations' queue size statistics
  inspectionQueueSizeAcc <- 
    runEventInStartTime $
    newQueueSizeAccumulator inspectionQueue
  -- the adjustment stations' queue size statistics
  adjustmentQueueSizeAcc <- 
    runEventInStartTime $
    newQueueSizeAccumulator adjustmentQueue
  -- create the inspection stations (servers)
  inspectionStations <-
    forM [1 .. inspectionStationCount] $ \_ ->
    newInspectionStation
  -- create the adjustment stations (servers)
  adjustmentStations <-
    forM [1 .. adjustmentStationCount] $ \_ ->
    newAdjustmentStation
  -- a processor loop for the inspection stations' queue
  let inspectionQueueProcessorLoop =
        queueProcessorLoopParallel
        (liftEvent . enqueue inspectionQueue)
        (dequeue inspectionQueue)
        inspectionProcessor
        (adjustmentQueueProcessor >>> adjustmentProcessor)
  -- a processor for the adjustment stations' queue
  let adjustmentQueueProcessor =
        queueProcessor
        (liftEvent . enqueue adjustmentQueue)
        (dequeue adjustmentQueue)
  -- a parallel work of the inspection stations
  let inspectionProcessor =
        processorParallel (map serverProcessor inspectionStations)
  -- a parallel work of the adjustment stations
  let adjustmentProcessor =
        processorParallel (map serverProcessor adjustmentStations)
  -- the entire processor from input to output
  let entireProcessor =
        arrivalTimerProcessor inputArrivalTimer >>>
        inspectionQueueProcessorLoop >>>
        arrivalTimerProcessor outputArrivalTimer
  -- start simulating the model
  runProcessInStartTime $
    sinkStream $ runProcessor entireProcessor inputStream
  -- return the experiment data
  experimentDataInStartTime
    [("t", seriesEntity "time" time),
     ("incomingArrivalTimer", 
      seriesEntity "incoming arival processing time" $ 
      arrivalProcessingTime inputArrivalTimer),
     ("outgoingArrivalTimer", 
      seriesEntity "outgoing arival processing time" $ 
      arrivalProcessingTime outputArrivalTimer),
     ("incomingArrivalCount", 
      seriesEntity "incoming arivals" $ 
      fmap samplingStatsCount $ arrivalProcessingTime inputArrivalTimer),
     ("outgoingArrivalCount", 
      seriesEntity "outgoing arivals" $ 
      fmap samplingStatsCount $ arrivalProcessingTime outputArrivalTimer),
     ("inspectionStationProcessingFactor",
      seriesEntity "the processing factor for the inspection stations" $
      map serverProcessingFactor inspectionStations),
     ("adjustmentStationProcessingFactor",
      seriesEntity "the processing factor for the adjustment stations" $
      map serverProcessingFactor adjustmentStations),
     ("inspectionQueueSize",
      seriesEntity "the inspection queue size" $
      queueCount inspectionQueue),
     ("adjustmentQueueSize",
      seriesEntity "the adjustment queue size" $
      queueCount adjustmentQueue),
     ("inspectionQueueWaitTime",
      seriesEntity "the inspection queue wait time" $
      queueWaitTime inspectionQueue),
     ("adjustmentQueueWaitTime",
      seriesEntity "the adjustment queue wait time" $
      queueWaitTime adjustmentQueue)]

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    -- experimentRunCount = 10,
    experimentTitle = "Inspection and Adjustment Stations on a Production Line (the Monte-Carlo simulation)",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Arrivals",
         finalStatsSeries = ["incomingArrivalCount",
                             "outgoingArrivalCount",
                             "outgoingArrivalTimer"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The processing factor (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "inspectionStationProcessingFactor",
                                 Right "adjustmentStationProcessingFactor"] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "The processing factor (histogram)",
         finalHistogramWidth = 1000,
         finalHistogramSeries = ["inspectionStationProcessingFactor",
                                 "adjustmentStationProcessingFactor"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The processing factor (statistics)",
         finalStatsSeries = ["inspectionStationProcessingFactor",
                             "adjustmentStationProcessingFactor"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The queue size (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "inspectionQueueSize",
                                 Right "adjustmentQueueSize"] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "The queue size (histogram)",
         finalHistogramWidth = 1000,
         finalHistogramSeries = ["inspectionQueueSize",
                                 "adjustmentQueueSize"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The queue size (statistics)",
         finalStatsSeries = ["inspectionQueueSize",
                             "adjustmentQueueSize"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The queue wait time (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "inspectionQueueWaitTime",
                                 Right "adjustmentQueueWaitTime"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The queue wait time (statistics)",
         finalStatsSeries = ["inspectionQueueWaitTime",
                             "adjustmentQueueWaitTime"] } ] }
       
main = runExperiment experiment model
