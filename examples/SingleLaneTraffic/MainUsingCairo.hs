
-- To run, package aivika-experiment-cairo must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Chart
import Simulation.Aivika.Experiment.Chart.Backend.Cairo

import Graphics.Rendering.Chart.Backend.Cairo

import Model
import Experiment

main = do
  runExperimentParallel experiment generators (WebPageRenderer (CairoRenderer PNG) experimentFilePath) model1
  runExperimentParallel experiment generators (WebPageRenderer (CairoRenderer PNG) experimentFilePath) model2
  runExperimentParallel experiment generators (WebPageRenderer (CairoRenderer PNG) experimentFilePath) model3
