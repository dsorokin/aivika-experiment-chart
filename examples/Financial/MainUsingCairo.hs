
-- To run, package aivika-experiment-cairo must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.CairoRenderer

import Graphics.Rendering.Chart.Backend.Cairo

import Model
import Experiment

main = do
  
  -- run the ordinary simulation
  putStrLn "*** The simulation with default parameters..."
  runExperiment singleExperiment (CairoRenderer PNG) (model defaultParams)
  putStrLn ""

  -- run the Monte-Carlo simulation
  putStrLn "*** The Monte-Carlo simulation..."
  randomParams >>= runExperimentParallel monteCarloExperiment (CairoRenderer PNG) . model
