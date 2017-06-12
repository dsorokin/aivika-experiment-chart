
{-
        GENERATE 2000,500,,,1
        GATE NI PROF,Busy
        PREEMPT PROF,PR,Add,5
        ADVANCE (Exponential(1,0,200))
        RETURN PROF
Busy    TERMINATE

        GENERATE 2000,500
        QUEUE LINE
        SEIZE PROF
        DEPART LINE
        ADVANCE (Exponential(1,0,1000))
LetGo   RELEASE PROF
        TERMINATE

Add     ASSIGN 5+,300
        ADVANCE P5
        TRANSFER ,LetGo

        GENERATE 10000000
        TERMINATE 1

        START 1
 -}
 
module Model (model) where

import Prelude hiding (id)

import Control.Category
import Control.Monad.Trans

import Data.Maybe

import Simulation.Aivika
import Simulation.Aivika.GPSS
import qualified Simulation.Aivika.GPSS.Queue as Q

model :: Simulation Results
model =
  do line <- runEventInStartTime Q.newQueue
     prof <- runEventInStartTime newFacility

     let phoneCallStream = randomUniformStream (2000 - 500) (2000 + 500)
         studentStream   = randomUniformStream (2000 - 500) (2000 + 500)

     let phoneCalls     = streamGeneratorBlock phoneCallStream 1
         phoneCallChain =
           Block (\a ->
                   do f <- liftEvent (facilityInterrupted prof)
                      if f
                        then blockProcess (transferBlock busy) a
                        else return a) >>>
           preemptBlock prof
           (PreemptBlockMode { preemptBlockPriorityMode = True,
                               preemptBlockTransfer     = Just add,
                               -- preemptBlockTransfer     = Nothing,
                               preemptBlockRemoveMode   = False }) >>>
           advanceBlock (randomExponentialProcess_ 200) >>>
           returnBlock prof >>>
           busy
         busy           = terminateBlock
         
         students       = streamGeneratorBlock studentStream 0
         studentChain   =
           queueBlock line 1 >>>
           seizeBlock prof >>>
           departBlock line 1 >>>
           advanceBlock (randomExponentialProcess_ 1000) >>>
           letGo
         letGo          =
           releaseBlock prof >>>
           terminateBlock
         add dt0        =
           let dt = maybe 0 id dt0
           in advanceBlock (holdProcess (dt + 300)) >>>
              transferBlock letGo

     runProcessInStartTime $
       runGeneratorBlock phoneCalls phoneCallChain 

     runProcessInStartTime $
       runGeneratorBlock students studentChain 

     runEventInStartTime $
       enqueueEvent 100000 $
       do Q.resetQueue line
          resetFacility prof
  
     return $
       results
       [resultSource "line" "Line" line,
        resultSource "prof" "Prof" prof]
