
-- |
-- Module     : Simulation.Aivika.Experiment.Chart.ChartRenderer
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines a type class for rendering charts.
--

module Simulation.Aivika.Experiment.Chart.ChartRenderer
       (ChartRenderer(..)) where

import Graphics.Rendering.Chart

import Simulation.Aivika.Experiment

-- | A type class of chart renderers.
class FileRenderer r => ChartRenderer r where

  -- | The file extension used when rendering.
  renderableFileExtension :: r -> String

  -- | Generate an image file for the given chart, at the specified path.
  -- The width and height are passed in the second argument to the function.
  renderChart :: r -> (Int, Int) -> Renderable a -> FilePath -> IO (PickFn a)