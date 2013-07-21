
-- This is a model of the Furnace. It is described in different sources [1, 2].
--
-- [1] { add a foreign source in English }
--
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
--
-- This model is often used in the literature as an example of combined
-- continuous-discrete simulation but this is not a point here. It illustrates
-- how the time-driven and process-oriented simulation models can be combined
-- based on the common event queue. It still uses the differential equation but
-- it is modeled directly [3] with help of the Euler method within the time-driven
-- part of the combined model.
--
-- [3] The time bounds for such an equation are much smaller than that ones which are defined
--     by the specs. Therefore there is no sense to use the 'integ' function as it would be
--     very slow because of large allocating memory for each integral, although it is possible.
--
--     However, you can still combine the differential (and difference) equations with the DES and
--     agent-based models. The integral (as well as any 'Dynamics' computation) can be used directly
--     in the DES sub-model. But to update something from the DES sub-model that could be used aready
--     in the differential equations, you should save data with help of types 'Var' or 'UVar' as they
--     keep all the history of their past values. Also the values of these two types are managed by
--     the event queue that allows synchronizing them with the DES sub-model.
--
-- To define the external parameters for the Monte-Carlo simulation, see the Financial model.
--
-- To enable the parallel simulation, you should compile it
-- with option -threaded and then pass in other options +RTS -N2 -RTS
-- to the executable if you have a dual core processor without
-- hyper-threading. Also you can increase the number
-- of parallel threads via option -N if you have a more modern
-- processor.

import Data.Maybe
import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.UVar
import Simulation.Aivika.Dynamics.Process
import Simulation.Aivika.Dynamics.Random
import Simulation.Aivika.Statistics

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.ExperimentSpecsView
import Simulation.Aivika.Experiment.FinalStatsView
import Simulation.Aivika.Experiment.DeviationChartView
import Simulation.Aivika.Experiment.FinalHistogramView

import qualified Simulation.Aivika.Queue as Q

-- | The simulation specs.
specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                -- spcStopTime = 300.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4 }
        
-- | Return an exponentially distributed random value with mean 
-- 1 / @lambda@, where @lambda@ is a parameter of the function.
exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)
     
-- | Return a random initial temperature of the item.     
temprnd :: IO Double
temprnd =
  do x <- getStdRandom random
     return (400.0 + (600.0 - 400.0) * x)

-- | Represents the furnace.
data Furnace = 
  Furnace { furnaceQueue :: EventQueue,
            -- ^ The event queue.
            furnaceNormalGen :: IO Double,
            -- ^ The normal random number generator.
            furnacePits :: [Pit],
            -- ^ The pits for ingots.
            furnacePitCount :: UVar Int,
            -- ^ The count of active pits with ingots.
            furnacePitCountStats :: Ref (SamplingStats Int),
            -- ^ The statistics about the active pits.
            furnaceAwaitingIngots :: Q.Queue Ingot,
            -- ^ The awaiting ingots in the queue.
            furnaceQueueCount :: UVar Int,
            -- ^ The queue count.
            furnaceQueueCountStats :: Ref (SamplingStats Int),
            -- ^ The statistics about the queue count.
            furnaceWaitCount :: Ref Int,
            -- ^ The count of awaiting ingots.
            furnaceWaitTime :: Ref Double,
            -- ^ The wait time for all loaded ingots.
            furnaceHeatingTime :: Ref Double,
            -- ^ The heating time for all unloaded ingots.
            furnaceTemp :: Ref Double,
            -- ^ The furnace temperature.
            furnaceTotalCount :: Ref Int,
            -- ^ The total count of ingots.
            furnaceLoadCount :: Ref Int,
            -- ^ The count of loaded ingots.
            furnaceUnloadCount :: Ref Int,
            -- ^ The count of unloaded ingots.
            furnaceUnloadTemps :: Ref [Double]
            -- ^ The temperatures of all unloaded ingots.
            }

-- | A pit in the furnace to place the ingots.
data Pit = 
  Pit { pitQueue :: EventQueue,
        -- ^ The bound dynamics queue.
        pitIngot :: Ref (Maybe Ingot),
        -- ^ The ingot in the pit.
        pitTemp :: Ref Double
        -- ^ The ingot temperature in the pit.
        }

data Ingot = 
  Ingot { ingotFurnace :: Furnace,
          -- ^ Return the furnace.
          ingotReceiveTime :: Double,
          -- ^ The time at which the ingot was received.
          ingotReceiveTemp :: Double,
          -- ^ The temperature with which the ingot was received.
          ingotLoadTime :: Double,
          -- ^ The time of loading in the furnace.
          ingotLoadTemp :: Double,
          -- ^ The temperature when the ingot was loaded in the furnace.
          ingotCoeff :: Double
          -- ^ The heating coefficient.
          }

-- | Create a furnace.
newFurnace :: EventQueue -> Simulation Furnace
newFurnace queue =
  do normalGen <- liftIO normalGen
     pits <- sequence [newPit queue | i <- [1..10]]
     pitCount <- newUVar queue 0
     pitCountStats <- newRef queue emptySamplingStats
     awaitingIngots <- liftIO Q.newQueue
     queueCount <- newUVar queue 0
     queueCountStats <- newRef queue emptySamplingStats
     waitCount <- newRef queue 0
     waitTime <- newRef queue 0.0
     heatingTime <- newRef queue 0.0
     h <- newRef queue 1650.0
     totalCount <- newRef queue 0
     loadCount <- newRef queue 0
     unloadCount <- newRef queue 0
     unloadTemps <- newRef queue []
     return Furnace { furnaceQueue = queue,
                      furnaceNormalGen = normalGen,
                      furnacePits = pits,
                      furnacePitCount = pitCount,
                      furnacePitCountStats = pitCountStats,
                      furnaceAwaitingIngots = awaitingIngots,
                      furnaceQueueCount = queueCount,
                      furnaceQueueCountStats = queueCountStats,
                      furnaceWaitCount = waitCount,
                      furnaceWaitTime = waitTime,
                      furnaceHeatingTime = heatingTime,
                      furnaceTemp = h,
                      furnaceTotalCount = totalCount,
                      furnaceLoadCount = loadCount, 
                      furnaceUnloadCount = unloadCount, 
                      furnaceUnloadTemps = unloadTemps }

-- | Create a new pit.
newPit :: EventQueue -> Simulation Pit
newPit queue =
  do ingot <- newRef queue Nothing
     h' <- newRef queue 0.0
     return Pit { pitQueue = queue,
                  pitIngot = ingot,
                  pitTemp  = h' }

-- | Create a new ingot.
newIngot :: Furnace -> Dynamics Ingot
newIngot furnace =
  do t  <- time
     xi <- liftIO $ furnaceNormalGen furnace
     h' <- liftIO temprnd
     let c = 0.1 + (0.05 + xi * 0.01)
     return Ingot { ingotFurnace = furnace,
                    ingotReceiveTime = t,
                    ingotReceiveTemp = h',
                    ingotLoadTime = t,
                    ingotLoadTemp = h',
                    ingotCoeff = c }

-- | Heat the ingot up in the pit if there is such an ingot.
heatPitUp :: Pit -> Dynamics ()
heatPitUp pit =
  do ingot <- readRef (pitIngot pit)
     case ingot of
       Nothing -> 
         return ()
       Just ingot -> do
         
         -- update the temperature of the ingot.
         let furnace = ingotFurnace ingot
         dt' <- dt
         h'  <- readRef (pitTemp pit)
         h   <- readRef (furnaceTemp furnace)
         writeRef (pitTemp pit) $ 
           h' + dt' * (h - h') * ingotCoeff ingot

-- | Check whether there are ready ingots in the pits.
ingotsReady :: Furnace -> Dynamics Bool
ingotsReady furnace =
  fmap (not . null) $ 
  filterM (fmap (>= 2200.0) . readRef . pitTemp) $ 
  furnacePits furnace

-- | Try to unload the ready ingot from the specified pit.
tryUnloadPit :: Furnace -> Pit -> Dynamics ()
tryUnloadPit furnace pit =
  do h' <- readRef (pitTemp pit)
     when (h' >= 2000.0) $
       do Just ingot <- readRef (pitIngot pit)  
          unloadIngot ingot pit

-- | Try to load an awaiting ingot in the specified empty pit.
tryLoadPit :: Furnace -> Pit -> Dynamics ()       
tryLoadPit furnace pit =
  do let ingots = furnaceAwaitingIngots furnace
     flag <- liftIO $ Q.queueNull ingots
     unless flag $
       do ingot <- liftIO $ Q.queueFront ingots
          liftIO $ Q.dequeue ingots
          t' <- time
          modifyUVar (furnaceQueueCount furnace) (+ (-1))
          c <- readUVar (furnaceQueueCount furnace)
          modifyRef (furnaceQueueCountStats furnace) $
            addSamplingStats c
          loadIngot (ingot { ingotLoadTime = t',
                             ingotLoadTemp = 400.0 }) pit
              
-- | Unload the ingot from the specified pit.       
unloadIngot :: Ingot -> Pit -> Dynamics ()
unloadIngot ingot pit = 
  do h' <- readRef (pitTemp pit)
     writeRef (pitIngot pit) Nothing
     writeRef (pitTemp pit) 0.0
     
     -- count the active pits
     let furnace = ingotFurnace ingot
     count <- readUVar (furnacePitCount furnace)
     writeUVar (furnacePitCount furnace) (count - 1)
     modifyRef (furnacePitCountStats furnace) $
       addSamplingStats (count - 1)
     
     -- how long did we heat the ingot up?
     t' <- time
     modifyRef (furnaceHeatingTime furnace)
       (+ (t' - ingotLoadTime ingot))
     
     -- what is the temperature of the unloaded ingot?
     modifyRef (furnaceUnloadTemps furnace) (h' :)
     
     -- count the unloaded ingots
     modifyRef (furnaceUnloadCount furnace) (+ 1)
     
-- | Load the ingot in the specified pit
loadIngot :: Ingot -> Pit -> Dynamics ()
loadIngot ingot pit =
  do writeRef (pitIngot pit) $ Just ingot
     writeRef (pitTemp pit) $ ingotLoadTemp ingot
     
     -- count the active pits
     let furnace = ingotFurnace ingot
     count <- readUVar (furnacePitCount furnace)
     writeUVar (furnacePitCount furnace) (count + 1)
     modifyRef (furnacePitCountStats furnace) $
       addSamplingStats (count + 1)
     
     -- decrease the furnace temperature
     h <- readRef (furnaceTemp furnace)
     let h' = ingotLoadTemp ingot
         dh = - (h - h') / fromInteger (toInteger (count + 1))
     writeRef (furnaceTemp furnace) $ h + dh

     -- how long did we keep the ingot in the queue?
     t' <- time
     when (ingotReceiveTime ingot < t') $
       do modifyRef (furnaceWaitCount furnace) (+ 1) 
          modifyRef (furnaceWaitTime furnace)
            (+ (t' - ingotReceiveTime ingot))

     -- count the loaded ingots
     modifyRef (furnaceLoadCount furnace) (+ 1)
  
-- | Start iterating the furnace processing through the event queue.
startIteratingFurnace :: Furnace -> Dynamics ()
startIteratingFurnace furnace = 
  let queue = furnaceQueue furnace
      pits = furnacePits furnace
  in enqueueWithIntegTimes queue $
     do ready <- ingotsReady furnace
        when ready $ 
          do mapM_ (tryUnloadPit furnace) pits
             pits' <- emptyPits furnace
             mapM_ (tryLoadPit furnace) pits'
        mapM_ heatPitUp pits
        
        -- update the temperature of the furnace
        dt' <- dt
        h   <- readRef (furnaceTemp furnace)
        writeRef (furnaceTemp furnace) $
          h + dt' * (2600.0 - h) * 0.2

-- | Return all empty pits.
emptyPits :: Furnace -> Dynamics [Pit]
emptyPits furnace =
  filterM (fmap isNothing . readRef . pitIngot) $
  furnacePits furnace

-- | Accept a new ingot.
acceptIngot :: Furnace -> Dynamics ()
acceptIngot furnace =
  do ingot <- newIngot furnace
     
     -- counting
     modifyRef (furnaceTotalCount furnace) (+ 1)
     
     -- check what to do with the new ingot
     count <- readUVar (furnacePitCount furnace)
     if count >= 10
       then do let ingots = furnaceAwaitingIngots furnace
               liftIO $ Q.enqueue ingots ingot
               modifyUVar (furnaceQueueCount furnace) (+ 1)
               c <- readUVar (furnaceQueueCount furnace)
               modifyRef (furnaceQueueCountStats furnace) $
                 addSamplingStats c
       else do pit:_ <- emptyPits furnace
               loadIngot ingot pit
       
-- | Process the furnace.
processFurnace :: Furnace -> Process ()
processFurnace furnace =
  do delay <- liftIO $ exprnd (1.0 / 2.5)
     holdProcess delay
     -- we have got a new ingot
     liftDynamics $ acceptIngot furnace
     -- repeat it again
     processFurnace furnace

-- | Initialize the furnace.
initializeFurnace :: Furnace -> Dynamics ()
initializeFurnace furnace =
  do x1 <- newIngot furnace
     x2 <- newIngot furnace
     x3 <- newIngot furnace
     x4 <- newIngot furnace
     x5 <- newIngot furnace
     x6 <- newIngot furnace
     let p1 : p2 : p3 : p4 : p5 : p6 : ps = 
           furnacePits furnace
     loadIngot (x1 { ingotLoadTemp = 550.0 }) p1
     loadIngot (x2 { ingotLoadTemp = 600.0 }) p2
     loadIngot (x3 { ingotLoadTemp = 650.0 }) p3
     loadIngot (x4 { ingotLoadTemp = 700.0 }) p4
     loadIngot (x5 { ingotLoadTemp = 750.0 }) p5
     loadIngot (x6 { ingotLoadTemp = 800.0 }) p6
     writeRef (furnaceTotalCount furnace) 6
     writeRef (furnaceTemp furnace) 1650.0
     
-- | The simulation model that returns experimental data.
model :: Simulation ExperimentData
model =
  do queue <- newQueue
     furnace <- newFurnace queue
     pid <- newProcessID queue

     -- initialize the furnace and start its iterating in start time
     runDynamicsInStartTime $
       do initializeFurnace furnace
          startIteratingFurnace furnace
     
     -- start accepting input ingots by launching the process
     runDynamicsInStartTime $
       do t0 <- starttime
          runProcess (processFurnace furnace) pid t0
     
     experimentDataInStartTime queue $
       [(totalIngotCountName,
         seriesEntity "total ingot count" $
         furnaceTotalCount furnace),
             
        (loadedIngotCountName,
         seriesEntity "loaded ingot count" $
         furnaceLoadCount furnace),
             
        (readyIngotCountName,
         seriesEntity "ready ingot count" $
         furnaceUnloadCount furnace),

        (awaitedIngotCountName,
         seriesEntity "awaited in the queue ingot count" $
         furnaceWaitCount furnace),

        (readyIngotTempsName,
         seriesEntity "the temperature of ready ingot" $
         furnaceUnloadTemps furnace),
                
        (pitCountName,
         seriesEntity "the used pit count" $
         readUVar $ furnacePitCount furnace),
              
        (queueCountName,
         seriesEntity "the queue size" $
         readUVar $ furnaceQueueCount furnace),
              
        (meanWaitTimeName,
         seriesEntity "the mean wait time" $
         readRef (furnaceWaitTime furnace) /
         fmap fromIntegral (readRef (furnaceWaitCount furnace))),

        (meanHeatingTimeName,
         seriesEntity "the mean heating time" $
         readRef (furnaceHeatingTime furnace) /
         fmap fromIntegral (readRef (furnaceUnloadCount furnace))) ]
              
totalIngotCountName    = "totalIngotCount"
loadedIngotCountName   = "loadedIngotCount"
readyIngotCountName    = "readyIngotCount"
awaitedIngotCountName  = "awaitedIngotCount"
readyIngotTempsName    = "readyIngotTemps"
pitCountName           = "pitCount"
queueCountName         = "queueCount"
meanWaitTimeName       = "the mean wait time in the queue"
meanHeatingTimeName    = "the mean heating time"

-- | The experiment.
experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    -- experimentRunCount = 1000,
    experimentRunCount = 100,
    experimentTitle = "The Furnace model (the Monte-Carlo simulation)",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 1.1",
         deviationChartPlotTitle = "The total, loaded and ready ingot counts",
         deviationChartSeries = [Right totalIngotCountName,
                                 Right loadedIngotCountName,
                                 Right readyIngotCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 1.2",
         deviationChartPlotTitle = "The awaited in the queue ingot count",
         deviationChartSeries = [Right awaitedIngotCountName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 1.1",
         finalHistogramPlotTitle = "The distribution of total, loaded and ready " ++
                                   "ingot counts in the final time point.",
         finalHistogramSeries = [totalIngotCountName,
                                 loadedIngotCountName,
                                 readyIngotCountName] },
       
       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 1.2",
         finalHistogramPlotTitle = "The distribution of the awaited in the queue " ++
                                   "ingot count in the final time point.",
         finalHistogramSeries = [awaitedIngotCountName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 1",
         finalStatsDescription = "The summary of total, loaded, ready and awaited in " ++
                                 "the queue ingot counts in the final time point.",
         finalStatsSeries = [totalIngotCountName,
                             loadedIngotCountName,
                             readyIngotCountName,
                             awaitedIngotCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 2",
         deviationChartPlotTitle = "The used pit count",
         deviationChartSeries = [Right pitCountName] },
       
       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 2",
         finalHistogramPlotTitle = "The distribution of the used pit count " ++
                                   "in the final simulation time point.",
         finalHistogramSeries = [pitCountName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 2",
         finalStatsDescription = "The summary of the used pit count in the final time point.",
         finalStatsSeries = [pitCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 3",
         deviationChartPlotTitle = "The queue size",
         deviationChartSeries = [Right queueCountName] },
       
       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 3",
         finalHistogramPlotTitle = "The distribution of the queue size " ++
                                   "in the final simulation time point.",
         finalHistogramSeries = [queueCountName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 3",
         finalStatsDescription = "The summary of the queue size in the final time point.",
         finalStatsSeries = [queueCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 4",
         deviationChartPlotTitle = "The mean wait time",
         deviationChartSeries = [Right meanWaitTimeName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 4",
         finalHistogramPlotTitle = "The distribution of the mean wait time " ++
                                   "in the final simulation time point.",
         finalHistogramSeries = [meanWaitTimeName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 4",
         finalStatsDescription = "The summary of the mean wait time in " ++
                                 "the final simulation time point.",
         finalStatsSeries = [meanWaitTimeName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 5",
         deviationChartPlotTitle = "The mean heating time",
         deviationChartSeries = [Right meanHeatingTimeName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 5",
         finalHistogramPlotTitle = "The distribution of the mean heating time " ++
                                   "in the final simulation time point.",
         finalHistogramSeries = [meanHeatingTimeName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 5",
         finalStatsDescription = "The summary of the mean heating time in " ++
                                 "the final simulation time point.",
         finalStatsSeries = [meanHeatingTimeName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 6",
         deviationChartPlotTitle = "The ready ingot temperature",
         deviationChartSeries = [Right readyIngotTempsName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 6",
         finalHistogramPlotTitle = "The distribution of the ready ingot temperature " ++
                                   "in the final simulation time point.",
         finalHistogramSeries = [readyIngotTempsName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 6",
         finalStatsDescription = "The summary of the ready ingot temperature in " ++
                                 "the final simulation time point.",
         finalStatsSeries = [readyIngotTempsName] }
      ] }

-- | The main program that launches the simulation experiment to produce the HTML file.
main = runExperimentParallel experiment model
