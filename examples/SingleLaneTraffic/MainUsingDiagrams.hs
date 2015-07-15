
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Backend.Diagrams

import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.Map as M

import Model
import Experiment

main = do
  runExperiment experiment generators (WebPageRenderer $ DiagramsRenderer SVG M.empty) model1
  runExperiment experiment generators (WebPageRenderer $ DiagramsRenderer SVG M.empty) model2
  runExperiment experiment generators (WebPageRenderer $ DiagramsRenderer SVG M.empty) model3