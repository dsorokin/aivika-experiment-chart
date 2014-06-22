
{-# LANGUAGE RecursiveDo #-}

module Model (model) where

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Experiment

model :: Simulation ExperimentData
model =
  mdo a <- integ (- ka * a) 100
      b <- integ (ka * a - kb * b) 0
      c <- integ (kb * b) 0
      let ka = 1
          kb = 1
      experimentDataInStartTime
        [("t", seriesEntity "time" time),
         ("a", seriesEntity "a" a),
         ("b", seriesEntity "b" b),
         ("c", seriesEntity "c" c)]
