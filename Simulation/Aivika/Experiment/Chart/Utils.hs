
-- |
-- Module     : Simulation.Aivika.Experiment.Chart.Utils
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- The module defines some utilities used in the charting.
--

module Simulation.Aivika.Experiment.Chart.Utils
       (colourisePlotLines,
        colourisePlotFillBetween,
        colourisePlotBars) where

import Control.Lens

import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart

-- | Colourise the plot lines.
colourisePlotLines :: [PlotLines x y -> PlotLines x y]
colourisePlotLines = map mkstyle $ cycle defaultColorSeq
  where mkstyle c = plot_lines_style . line_color .~ c

-- | Colourise the filling areas.
colourisePlotFillBetween :: [PlotFillBetween x y -> PlotFillBetween x y]
colourisePlotFillBetween = map mkstyle $ cycle defaultColorSeq
  where mkstyle c = plot_fillbetween_style .~ solidFillStyle (dissolve 0.4 c)
  
-- | Colourise the plot bars.
colourisePlotBars :: PlotBars x y -> PlotBars x y
colourisePlotBars = plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
  where mkstyle c = (solidFillStyle c, Just $ solidLine 1.0 $ opaque black)
  
