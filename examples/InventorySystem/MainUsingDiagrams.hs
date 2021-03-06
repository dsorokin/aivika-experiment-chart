
-- To run, package aivika-experiment-diagrams must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart
import Simulation.Aivika.Experiment.Chart.Backend.Diagrams

import Graphics.Rendering.Chart.Backend.Diagrams

import Model
import Experiment

main = 
  do fonts <- loadCommonFonts
     let renderer = DiagramsRenderer SVG (return fonts)
     runExperimentParallel experiment generators (WebPageRenderer renderer experimentFilePath) model
