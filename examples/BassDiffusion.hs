
import System.Random
import Data.Array
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Agent
import Simulation.Aivika.Dynamics.Ref

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.DeviationChartView
import Simulation.Aivika.Experiment.TimeSeriesView
import Simulation.Aivika.Experiment.ExperimentSpecsView

n = 100    -- the number of agents

advertisingEffectiveness = 0.011
contactRate = 100.0
adoptionFraction = 0.015

specs = Specs { spcStartTime = 0.0, 
                spcStopTime = 8.0,
                spcDT = 0.1,
                spcMethod = RungeKutta4 }

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 20,
    experimentDescription = "This is a famous Bass Diffusion model solved with help of the agent-based modelling.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       outputView $ defaultDeviationChartView {
         deviationChartSeries = [Left "potentialAdopters", 
                                 Left "adopters"] },
       outputView $ defaultTimeSeriesView {
         timeSeries = [Left "potentialAdopters", 
                       Left "adopters"] } ] }

exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)
     
boolrnd :: Double -> IO Bool
boolrnd p =
  do x <- getStdRandom random
     return (x <= p)

data Person = Person { personAgent :: Agent,
                       personPotentialAdopter :: AgentState,
                       personAdopter :: AgentState }
              
createPerson :: EventQueue -> Simulation Person              
createPerson q =    
  do agent <- newAgent q
     potentialAdopter <- newState agent
     adopter <- newState agent
     return Person { personAgent = agent,
                     personPotentialAdopter = potentialAdopter,
                     personAdopter = adopter }
       
createPersons :: EventQueue -> Simulation (Array Int Person)
createPersons q =
  do list <- forM [1 .. n] $ \i ->
       do p <- createPerson q
          return (i, p)
     return $ array (1, n) list
     
definePerson :: Person -> Array Int Person -> Ref Int -> Ref Int -> Simulation ()
definePerson p ps potentialAdopters adopters =
  do stateActivation (personPotentialAdopter p) $
       do modifyRef potentialAdopters $ \a -> a + 1
          -- add a timeout
          t <- liftIO $ exprnd advertisingEffectiveness 
          let st  = personPotentialAdopter p
              st' = personAdopter p
          addTimeout st t $ activateState st'
     stateActivation (personAdopter p) $ 
       do modifyRef adopters  $ \a -> a + 1
          -- add a timer that works while the state is active
          let t = liftIO $ exprnd contactRate    -- many times!
          addTimer (personAdopter p) t $
            do i <- liftIO $ getStdRandom $ randomR (1, n)
               let p' = ps ! i
               st <- agentState (personAgent p')
               when (st == Just (personPotentialAdopter p')) $
                 do b <- liftIO $ boolrnd adoptionFraction
                    when b $ activateState (personAdopter p')
     stateDeactivation (personPotentialAdopter p) $
       modifyRef potentialAdopters $ \a -> a - 1
     stateDeactivation (personAdopter p) $
       modifyRef adopters $ \a -> a - 1
        
definePersons :: Array Int Person -> Ref Int -> Ref Int -> Simulation ()
definePersons ps potentialAdopters adopters =
  forM_ (elems ps) $ \p -> 
  definePerson p ps potentialAdopters adopters
                               
activatePerson :: Person -> Dynamics ()
activatePerson p = activateState (personPotentialAdopter p)

activatePersons :: Array Int Person -> Dynamics ()
activatePersons ps =
  forM_ (elems ps) $ \p -> activatePerson p

model :: Simulation ExperimentData
model =
  do q <- newQueue
     potentialAdopters <- newRef q 0
     adopters <- newRef q 0
     ps <- createPersons q
     definePersons ps potentialAdopters adopters
     runDynamicsInStartTime $
       activatePersons ps
     experimentDataInStartTime q
       [("potentialAdopters",
         seriesEntity "Potential Adopters" 
         potentialAdopters),
        ("adopters",
         seriesEntity "Adopters"
         adopters)]

main = runExperiment experiment model
