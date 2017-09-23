
-- |
-- Module     : Simulation.Aivika.Experiment.Chart.Types
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines a type class for rendering charts.
--

module Simulation.Aivika.Experiment.Chart.Types
       (ChartRendering(..)) where

import Graphics.Rendering.Chart

import Simulation.Aivika.Experiment

-- | A type class of chart renderers.
class ChartRendering r where

  -- | The file extension used when rendering.
  renderableChartExtension :: r -> String

  -- | Generate an image file with the specified path for the given chart.
  -- The width and height are passed in the second argument to the function.
  renderChart :: r -> (Int, Int) -> FilePath -> Renderable c -> IO (PickFn c)

  -- | Return the rendering layout.
  renderingLayoutLR :: r -> LayoutLR Double Double Double -> LayoutLR Double Double Double

  -- | Return the rendering layout.
  renderingLayout :: r -> Layout Double Double -> Layout Double Double
