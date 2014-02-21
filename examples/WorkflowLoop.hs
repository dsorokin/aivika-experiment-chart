
{-# LANGUAGE RecursiveDo, Arrows #-}

-- This is a model of the workflow with a loop. Also there are two infinite queues.
--
-- It is described in different sources [1, 2]. So, this is chapter 8 of [2].
--
-- [1] { add a foreign source in English }
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

-- CAUTION: this model is not yet fully tested and it may contain logical errors.

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

-- the minimum delay for arriving the next TV set
minArrivalDelay = 3.5

-- the maximum delay for arriving the next TV set
maxArrivalDelay = 7.5

-- the minimum test time
minTestTime = 6

-- the maximum test time
maxTestTime = 12

-- the probability of passing the test
testPassingProb = 0.85

-- how many testers are there?
testerWorkplaceCount = 2

-- the minimum time of tuning the TV set 
-- that has not passed the test
minTuningTime = 20

-- the maximum time of tuning the TV set
-- that has not passed the test
maxTuningTime = 40

-- how many persons perform a tuning of TV sets?
tunerWorkplaceCount = 1

-- create an accumulator to gather the queue size statistics 
newQueueSizeAccumulator queue =
  newTimingStatsAccumulator $
  Signalable (queueCount queue) (queueCountChanged_ queue)

-- create a tester's workplace
newTesterWorkplace =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minTestTime maxTestTime)
     passed <- 
       liftParameter $
       randomTrue testPassingProb
     if passed
       then return $ Right a
       else return $ Left a 

-- create a tuner's workplace
newTunerWorkplace =
  newServer $ \a ->
  do holdProcess =<<
       (liftParameter $
        randomUniform minTuningTime maxTuningTime)
     return a
  
model :: Simulation ExperimentData
model = mdo
  -- it will just count the number of arivals
  inputArrivalTimer <- newArrivalTimer
  -- it will gather the statistics of the processing time
  outputArrivalTimer <- newArrivalTimer
  -- define a stream of input events
  let inputStream =
        randomUniformStream minArrivalDelay maxArrivalDelay 
  -- create a queue before the tester's work place
  testerQueue <- newFCFSQueue
  -- create a queue before the tuner's work place
  tunerQueue <- newFCFSQueue
  -- the tester's queue size statistics
  testerQueueSizeAcc <- 
    runEventInStartTime $
    newQueueSizeAccumulator testerQueue
  -- the tuner's queue size statistics
  tunerQueueSizeAcc <- 
    runEventInStartTime $
    newQueueSizeAccumulator tunerQueue
  -- create the tester's work places
  testerWorkplaces <- 
    forM [1 .. testerWorkplaceCount] $ \_ -> 
    newTesterWorkplace
  -- create the tuner's work places
  tunerWorkplaces <-
    forM [1 .. tunerWorkplaceCount] $ \_ -> 
    newTunerWorkplace
  -- a processor loop for the tester's queue
  let testerQueueProcessorLoop =
        queueProcessorLoopSeq
        (liftEvent . enqueue testerQueue)
        (dequeue testerQueue)
        testerProcessor
        (tunerQueueProcessor >>> tunerProcessor)
  -- a processor for the tuner's queue
  let tunerQueueProcessor =
        queueProcessor
        (liftEvent . enqueue tunerQueue)
        (dequeue tunerQueue)
  -- the parallel work of all the testers
  let testerProcessor =
        processorParallel $ 
        map serverProcessor testerWorkplaces
  -- the parallel work of all the tuners
  let tunerProcessor =
        processorParallel $
        map serverProcessor tunerWorkplaces
  -- the entire processor from input to output
  let entireProcessor =
        arrivalTimerProcessor inputArrivalTimer >>>
        testerQueueProcessorLoop >>>
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
     ("testerWorkplaceProcessingFactor",
      seriesEntity "the tester's workplace processing factor" $
      map serverProcessingFactor testerWorkplaces),
     ("tunerWorkplaceProcessingFactor",
      seriesEntity "the tuner's workplace processing factor" $
      map serverProcessingFactor tunerWorkplaces),
     ("testerQueueSize",
      seriesEntity "the tester's queue size" $
      queueCount testerQueue),
     ("tunerQueueSize",
      seriesEntity "the tuner's queue size" $
      queueCount tunerQueue)]

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    -- experimentRunCount = 10,
    experimentTitle = "The Workflow Loop model (the Monte-Carlo simulation)",
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
         deviationChartSeries = [Right "testerWorkplaceProcessingFactor",
                                 Right "tunerWorkplaceProcessingFactor"] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "The processing factor (histogram)",
         finalHistogramWidth = 1000,
         finalHistogramSeries = ["testerWorkplaceProcessingFactor",
                                 "tunerWorkplaceProcessingFactor"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The processing factor (statistics)",
         finalStatsSeries = ["testerWorkplaceProcessingFactor",
                             "tunerWorkplaceProcessingFactor"] },
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The queue size (chart)",
         deviationChartWidth = 1000,
         deviationChartSeries = [Right "testerQueueSize",
                                 Right "tunerQueueSize"] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "The queue size (histogram)",
         finalHistogramWidth = 1000,
         finalHistogramSeries = ["testerQueueSize",
                                 "tunerQueueSize"] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "The queue size (statistics)",
         finalStatsSeries = ["testerQueueSize",
                             "tunerQueueSize"] } ] }
       
main = runExperiment experiment model
