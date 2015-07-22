
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart.Backend.Diagrams

import Graphics.Rendering.Chart.Backend.Diagrams

import Model
import Experiment

main = runExperimentParallel experiment generators (WebPageRenderer $ DiagramsRenderer SVG loadCommonFonts) model
