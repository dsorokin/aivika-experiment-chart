
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

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Event
import Simulation.Aivika.Ref
import Simulation.Aivika.Process
import Simulation.Aivika.Random
import Simulation.Aivika.Signal
import Simulation.Aivika.Queue.Infinite
import Simulation.Aivika.Statistics

import qualified Simulation.Aivika.DoubleLinkedList as DLL

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.ExperimentSpecsView
import Simulation.Aivika.Experiment.FinalStatsView
import Simulation.Aivika.Experiment.DeviationChartView
import Simulation.Aivika.Experiment.FinalHistogramView

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
  Furnace { furnaceNormalGen :: IO Double,
            -- ^ The normal random number generator.
            furnacePits :: [Pit],
            -- ^ The pits for ingots.
            furnacePitCount :: Ref Int,
            -- ^ The count of active pits with ingots.
            furnaceQueue :: FCFSQueue Ingot,
            -- ^ The furnace queue.
            furnaceUnloadedSource :: SignalSource (),
            -- ^ Notifies when the ingots have been
            -- unloaded from the furnace.
            furnaceHeatingTime :: Ref (SamplingStats Double),
            -- ^ The heating time for the ready ingots.
            furnaceTemp :: Ref Double,
            -- ^ The furnace temperature.
            furnaceReadyCount :: Ref Int,
            -- ^ The count of ready ingots.
            furnaceReadyTemps :: Ref [Double]
            -- ^ The temperatures of all ready ingots.
            }

-- | Notifies when the ingots have been unloaded from the furnace.
furnaceUnloaded :: Furnace -> Signal ()
furnaceUnloaded = publishSignal . furnaceUnloadedSource

-- | A pit in the furnace to place the ingots.
data Pit = 
  Pit { pitIngot :: Ref (Maybe Ingot),
        -- ^ The ingot in the pit.
        pitTemp :: Ref Double
        -- ^ The ingot temperature in the pit.
        }

data Ingot = 
  Ingot { ingotFurnace :: Furnace,
          -- ^ The furnace.
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
newFurnace :: Simulation Furnace
newFurnace =
  do normalGen <- liftIO newNormalGen
     pits <- sequence [newPit | i <- [1..10]]
     pitCount <- newRef 0
     queue <- newFCFSQueue
     heatingTime <- newRef emptySamplingStats
     h <- newRef 1650.0
     readyCount <- newRef 0
     readyTemps <- newRef []
     s <- newSignalSource
     return Furnace { furnaceNormalGen = normalGen,
                      furnacePits = pits,
                      furnacePitCount = pitCount,
                      furnaceQueue = queue,
                      furnaceUnloadedSource = s,
                      furnaceHeatingTime = heatingTime,
                      furnaceTemp = h,
                      furnaceReadyCount = readyCount, 
                      furnaceReadyTemps = readyTemps }

-- | Create a new pit.
newPit :: Simulation Pit
newPit =
  do ingot <- newRef Nothing
     h' <- newRef 0.0
     return Pit { pitIngot = ingot,
                  pitTemp  = h' }

-- | Create a new ingot.
newIngot :: Furnace -> Event Ingot
newIngot furnace =
  do t  <- liftDynamics time
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
heatPitUp :: Pit -> Event ()
heatPitUp pit =
  do ingot <- readRef (pitIngot pit)
     case ingot of
       Nothing -> 
         return ()
       Just ingot -> do
         
         -- update the temperature of the ingot.
         let furnace = ingotFurnace ingot
         dt' <- liftDynamics dt
         h'  <- readRef (pitTemp pit)
         h   <- readRef (furnaceTemp furnace)
         writeRef (pitTemp pit) $ 
           h' + dt' * (h - h') * ingotCoeff ingot

-- | Check whether there are ready ingots in the pits.
ingotsReady :: Furnace -> Event Bool
ingotsReady furnace =
  fmap (not . null) $ 
  filterM (fmap (>= 2200.0) . readRef . pitTemp) $ 
  furnacePits furnace

-- | Try to unload the ready ingot from the specified pit.
tryUnloadPit :: Furnace -> Pit -> Event ()
tryUnloadPit furnace pit =
  do h' <- readRef (pitTemp pit)
     when (h' >= 2000.0) $
       do Just ingot <- readRef (pitIngot pit)  
          unloadIngot furnace ingot pit

-- | Try to load an awaiting ingot in the specified empty pit.
tryLoadPit :: Furnace -> Pit -> Event ()       
tryLoadPit furnace pit =
  do ingot <- tryDequeue (furnaceQueue furnace)
     case ingot of
       Nothing ->
         return ()
       Just ingot ->
         do t' <- liftDynamics time
            loadIngot furnace (ingot { ingotLoadTime = t',
                                       ingotLoadTemp = 400.0 }) pit
              
-- | Unload the ingot from the specified pit.       
unloadIngot :: Furnace -> Ingot -> Pit -> Event ()
unloadIngot furnace ingot pit = 
  do h' <- readRef (pitTemp pit)
     writeRef (pitIngot pit) Nothing
     writeRef (pitTemp pit) 0.0

     -- count the active pits
     modifyRef (furnacePitCount furnace) (+ (- 1))
     
     -- how long did we heat the ingot up?
     t' <- liftDynamics time
     modifyRef (furnaceHeatingTime furnace) $
       addSamplingStats (t' - ingotLoadTime ingot)
     
     -- what is the temperature of the unloaded ingot?
     modifyRef (furnaceReadyTemps furnace) (h' :)
     
     -- count the ready ingots
     modifyRef (furnaceReadyCount furnace) (+ 1)
     
-- | Load the ingot in the specified pit
loadIngot :: Furnace -> Ingot -> Pit -> Event ()
loadIngot furnace ingot pit =
  do writeRef (pitIngot pit) $ Just ingot
     writeRef (pitTemp pit) $ ingotLoadTemp ingot

     -- count the active pits
     modifyRef (furnacePitCount furnace) (+ 1)
     count <- readRef (furnacePitCount furnace)
     
     -- decrease the furnace temperature
     h <- readRef (furnaceTemp furnace)
     let h' = ingotLoadTemp ingot
         dh = - (h - h') / fromIntegral count
     writeRef (furnaceTemp furnace) $ h + dh
 
-- | Start iterating the furnace processing through the event queue.
startIteratingFurnace :: Furnace -> Event ()
startIteratingFurnace furnace = 
  let pits = furnacePits furnace
  in enqueueEventWithIntegTimes $
     do -- try to unload ready ingots
        ready <- ingotsReady furnace
        when ready $ 
          do mapM_ (tryUnloadPit furnace) pits
             triggerSignal (furnaceUnloadedSource furnace) ()

        -- heat up
        mapM_ heatPitUp pits
        
        -- update the temperature of the furnace
        dt' <- liftDynamics dt
        h   <- readRef (furnaceTemp furnace)
        writeRef (furnaceTemp furnace) $
          h + dt' * (2600.0 - h) * 0.2

-- | Return all empty pits.
emptyPits :: Furnace -> Event [Pit]
emptyPits furnace =
  filterM (fmap isNothing . readRef . pitIngot) $
  furnacePits furnace

-- | This process takes ingots from the queue and then
-- loads them in the furnace.
loadingProcess :: Furnace -> Process ()
loadingProcess furnace =
  do ingot <- dequeue (furnaceQueue furnace)
     let wait :: Process ()
         wait =
           do count <- liftEvent $ readRef (furnacePitCount furnace)
              when (count >= 10) $
                do awaitSignal (furnaceUnloaded furnace)
                   wait
     wait
     --  take any empty pit and load it
     liftEvent $
       do pit: _ <- emptyPits furnace
          loadIngot furnace ingot pit
     -- repeat it again
     loadingProcess furnace
                  
-- | The input process that adds new ingots to the queue.
inputProcess :: Furnace -> Process ()
inputProcess furnace =
  do delay <- liftIO $ exprnd (1.0 / 2.5)
     holdProcess delay
     -- we have got a new ingot
     liftEvent $
       do ingot <- newIngot furnace
          enqueue (furnaceQueue furnace) ingot
     -- repeat it again
     inputProcess furnace

-- | Initialize the furnace.
initializeFurnace :: Furnace -> Event ()
initializeFurnace furnace =
  do x1 <- newIngot furnace
     x2 <- newIngot furnace
     x3 <- newIngot furnace
     x4 <- newIngot furnace
     x5 <- newIngot furnace
     x6 <- newIngot furnace
     let p1 : p2 : p3 : p4 : p5 : p6 : ps = 
           furnacePits furnace
     loadIngot furnace (x1 { ingotLoadTemp = 550.0 }) p1
     loadIngot furnace (x2 { ingotLoadTemp = 600.0 }) p2
     loadIngot furnace (x3 { ingotLoadTemp = 650.0 }) p3
     loadIngot furnace (x4 { ingotLoadTemp = 700.0 }) p4
     loadIngot furnace (x5 { ingotLoadTemp = 750.0 }) p5
     loadIngot furnace (x6 { ingotLoadTemp = 800.0 }) p6
     writeRef (furnaceTemp furnace) 1650.0
     
-- | The simulation model that returns experimental data.
model :: Simulation ExperimentData
model =
  do furnace <- newFurnace
     pid1 <- newProcessId
     pid2 <- newProcessId
  
     -- initialize the furnace and start its iterating in start time
     runEventInStartTime IncludingCurrentEvents $
       do initializeFurnace furnace
          startIteratingFurnace furnace
     
     -- generate randomly new input ingots
     runProcessInStartTime IncludingCurrentEvents
       pid1 (inputProcess furnace)

     -- load permanently the input ingots in the furnace
     runProcessInStartTime IncludingCurrentEvents
       pid2 (loadingProcess furnace)
     
     experimentDataInStartTime
       [(totalIngotCountName,
         seriesEntity "total ingot count" $
         queueStoreCount (furnaceQueue furnace)),
             
        (loadedIngotCountName,
         seriesEntity "loaded ingot count" $  -- actually, +/- 1
         queueOutputCount (furnaceQueue furnace)),
             
        (readyIngotCountName,
         seriesEntity "ready ingot count" $
         furnaceReadyCount furnace),

        (readyIngotTempsName,
         seriesEntity "the temperature of ready ingot" $
         furnaceReadyTemps furnace),
                
        (pitCountName,
         seriesEntity "the used pit count" $
         furnacePitCount furnace),
              
        (queueCountName,
         seriesEntity "the queue size" $
         queueCount (furnaceQueue furnace)),
              
        (meanWaitTimeName,
         seriesEntity "the mean wait time" $
         queueWaitTime (furnaceQueue furnace)),

        (meanHeatingTimeName,
         seriesEntity "the mean heating time" $
         furnaceHeatingTime furnace) ]
              
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
         deviationChartTitle = "Deviation Chart - 1",
         deviationChartPlotTitle = "The total, loaded and ready ingot counts",
         deviationChartSeries = [Right totalIngotCountName,
                                 Right loadedIngotCountName,
                                 Right readyIngotCountName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 1",
         finalHistogramPlotTitle = "The distribution of total, loaded and ready " ++
                                   "ingot counts in the final time point.",
         finalHistogramSeries = [totalIngotCountName,
                                 loadedIngotCountName,
                                 readyIngotCountName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 1",
         finalStatsDescription = "The summary of total, loaded and ready " ++
                                 "ingot counts in the final time point.",
         finalStatsSeries = [totalIngotCountName,
                             loadedIngotCountName,
                             readyIngotCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 2",
         deviationChartPlotTitle = "The used pit count",
         deviationChartSeries = [Right pitCountName] },
       
       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 2",
         finalHistogramPlotTitle = "The used pit count in the final time point.",
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
         finalHistogramPlotTitle = "The queue size in the final time point.",
         finalHistogramSeries = [queueCountName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 3",
         finalStatsDescription = "The summary of the queue size in the final time point.",
         finalStatsSeries = [queueCountName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 4",
         deviationChartPlotTitle = "The mean wait time",
         deviationChartSeries = [Right meanWaitTimeName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 4",
         finalStatsDescription = "The summary of the mean wait time in " ++
                                 "the final time point.",
         finalStatsSeries = [meanWaitTimeName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 5",
         deviationChartPlotTitle = "The mean heating time",
         deviationChartSeries = [Right meanHeatingTimeName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 5",
         finalStatsDescription = "The summary of the mean heating time in " ++
                                 "the final time point.",
         finalStatsSeries = [meanHeatingTimeName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "Deviation Chart - 6",
         deviationChartPlotTitle = "The ready ingot temperature",
         deviationChartSeries = [Right readyIngotTempsName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Final Histogram - 6",
         finalHistogramPlotTitle = "The ready ingot temperature in " ++
                                   "the final time point.",
         finalHistogramSeries = [readyIngotTempsName] },
       
       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Final Statistics - 6",
         finalStatsDescription = "The summary of the ready ingot temperature in " ++
                                 "the final time point.",
         finalStatsSeries = [readyIngotTempsName] }
      ] }

-- | The main program that launches the simulation experiment to produce the HTML file.
main = runExperimentParallel experiment model
