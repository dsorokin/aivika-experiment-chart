
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.DiagramsRenderer

import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.Map as M

import Model
import Experiment

main = do
  
  -- run the ordinary simulation
  putStrLn "*** The simulation with default parameters..."
  runExperiment singleExperiment (DiagramsRenderer SVG M.empty) (model defaultParams)
  putStrLn ""

  -- run the Monte-Carlo simulation
  putStrLn "*** The Monte-Carlo simulation..."
  randomParams >>= runExperimentParallel monteCarloExperiment (DiagramsRenderer SVG M.empty) . model
