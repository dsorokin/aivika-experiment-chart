
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Backend.Diagrams

import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.Map as M

import Model
import Experiment

main = do
  
  -- run the ordinary simulation
  putStrLn "*** The simulation with default parameters..."
  runExperiment
    singleExperiment singleGenerators
    (DiagramsRenderer SVG M.empty) (model defaultParams)
  putStrLn ""

  -- run the Monte-Carlo simulation
  putStrLn "*** The Monte-Carlo simulation..."
  randomParams >>= runExperimentParallel
    monteCarloExperiment monteCarloGenerators
    (DiagramsRenderer SVG M.empty) . model
