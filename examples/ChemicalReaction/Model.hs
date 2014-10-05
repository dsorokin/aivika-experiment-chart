
{-# LANGUAGE RecursiveDo #-}

module Model (model) where

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Experiment

model :: Simulation Results
model =
  mdo a <- integ (- ka * a) 100
      b <- integ (ka * a - kb * b) 0
      c <- integ (kb * b) 0
      let ka = 1
          kb = 1
      return $
        results
        [resultSource "t" "time" time,
         resultSource "a" "A" a,
         resultSource "b" "B" b,
         resultSource "c" "C" c]
