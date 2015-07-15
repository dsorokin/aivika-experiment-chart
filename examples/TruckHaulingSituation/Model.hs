
{-# LANGUAGE Arrows #-}

-- Example: A Truck Hauling Situation
--
-- It is described in different sources [1, 2]. So, this is chapter 9 of [2] and section 7.16 of [1].
--
-- [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
-- [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

module Model (model) where

import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Data.Monoid
import Data.List
import Data.Array

import Simulation.Aivika
import qualified Simulation.Aivika.Queue.Infinite as IQ

data Truck = Truck

data Pile = Pile

data Loader = Loader1
            | Loader2
              deriving (Eq, Ord, Show, Ix)

awaitQueuesNonEmpty q1 q2 q3 =
  do n1 <- liftEvent $ IQ.queueCount q1
     n2 <- liftEvent $ IQ.queueCount q2
     n3 <- liftEvent $ IQ.queueCount q3
     when (n1 == 0 || n2 == 0 || n3 == 0) $
       do let signal = IQ.queueCountChanged_ q1 <>
                       IQ.queueCountChanged_ q2 <>
                       IQ.queueCountChanged_ q3
          processAwait signal
          awaitQueuesNonEmpty q1 q2 q3

-- | The simulation model.
model :: Simulation Results
model = do
  truckQueue <- runEventInStartTime IQ.newFCFSQueue
  loadQueue <- runEventInStartTime IQ.newFCFSQueue
  loaderQueue <- runEventInStartTime IQ.newFCFSQueue
  loaderActvty1 <- newRandomExponentialActivity 14
  loaderActvty2 <- newRandomExponentialActivity 12
  let loaderActvties =
        array (Loader1, Loader2)
        [(Loader1, loaderActvty1),
         (Loader2, loaderActvty2)]
  let start :: Process ()
      start =
        do randomErlangProcess_ 4 2
           randomErlangProcess_ 4 2
           liftEvent $
             IQ.enqueue loadQueue Pile
           t <- liftDynamics time
           when (t <= 480) start
      actvty :: Net () ()
      actvty =
        (arrNet $ \() ->
         do awaitQueuesNonEmpty truckQueue loadQueue loaderQueue
            x1 <- IQ.dequeue truckQueue
            x2 <- IQ.dequeue loadQueue
            x3 <- IQ.dequeue loaderQueue
            return (x1, x3)) >>>
        (proc (truck, loader) ->
          case loader of
            Loader1 ->
              activityNet (loaderActvties ! Loader1) -< (truck, loader)
            Loader2 ->
              activityNet (loaderActvties ! Loader2) -< (truck, loader)) >>>
        (arrNet $ \(truck, loader) ->
          do spawnProcess $
               do holdProcess 5
                  liftEvent $
                    IQ.enqueue loaderQueue loader
             spawnProcess $
               do randomNormalProcess_ 22 3
                  randomUniformProcess_ 2 8
                  randomNormalProcess_ 18 3
                  liftEvent $
                    IQ.enqueue truckQueue truck
             return ())
  runEventInStartTime $
    do forM_ [1..4] $ \i ->
         IQ.enqueue truckQueue Truck
       IQ.enqueue loaderQueue Loader1
       IQ.enqueue loaderQueue Loader2
  runProcessInStartTime $ iterateNet actvty ()
  runProcessInStartTime $ iterateNet actvty ()
  runProcessInStartTime start
  return $
    results
    [ resultSource
     "loadQueue" "Queue Load"
     loadQueue,
     --
     resultSource
     "truckQueue" "Queue Trucks"
     truckQueue,
     --
     resultSource
     "loaderQueue" "Queue Loader"
     loaderQueue,
     --
     resultSource
     "loaderActvties" "Activity Loader"
     loaderActvties]
