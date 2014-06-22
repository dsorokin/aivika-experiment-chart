
-- To run, package aivika-experiment-cairo must be installed.

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.CairoRenderer

import Graphics.Rendering.Chart.Backend.Cairo

import Model
import Experiment

main = runExperiment experiment (CairoRenderer PNG) (model 51)
