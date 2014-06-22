
{-# LANGUAGE RecursiveDo #-}

module Model (model) where

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Experiment

model :: Simulation ExperimentData
model =
  mdo x <- memoRandomNormalDynamics 3 0.8
      sumX <- diffsum x 0
      sumX2 <- diffsum (x * x) 0
      
      -- it would be much more efficient to say:
      --   let n = fmap fromIntegral integIteration
      n <- diffsum 1 0

      let avg = ifDynamics (n .>. 0) (sumX / n) 0
      let std = ifDynamics (n .>. 1) (sqrt ((sumX2 - sumX * avg) / (n - 1))) 0
      
      experimentDataInStartTime
        [("t", seriesEntity "time" time),
         ("n", seriesEntity "n" n),
         ("x", seriesEntity "x" x),
         ("sumX", seriesEntity "sumX" sumX),
         ("sumX2", seriesEntity "sumX2" sumX2),
         ("avg", seriesEntity "avg" avg),
         ("std", seriesEntity "std" std)]
