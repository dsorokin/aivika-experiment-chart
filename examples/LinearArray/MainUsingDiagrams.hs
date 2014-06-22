
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.DiagramsRenderer

import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.Map as M

import Model
import Experiment

main = runExperiment experiment (DiagramsRenderer SVG M.empty) (model 51)
