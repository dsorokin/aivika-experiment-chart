
-- |
-- Module     : Simulation.Aivika.Experiment.Chart
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines some utilities used in the charting.
--

module Simulation.Aivika.Experiment.Chart
       (colourisePlotLines,
        colourisePlotFillBetween) where

import Data.Colour
import Data.Colour.Names
import Data.Accessor

import Graphics.Rendering.Chart

-- | Colourise the plot lines.
colourisePlotLines :: [PlotLines x y -> PlotLines x y]
colourisePlotLines =
  (plot_lines_style .> line_color ^= opaque blue) :
  (plot_lines_style .> line_color ^= opaque green) :
  (plot_lines_style .> line_color ^= opaque red) :
  (plot_lines_style .> line_color ^= opaque black) :
  (plot_lines_style .> line_color ^= opaque grey) :
  (plot_lines_style .> line_color ^= opaque purple) :
  (plot_lines_style .> line_color ^= opaque violet) :
  (plot_lines_style .> line_color ^= opaque darkblue) :
  (plot_lines_style .> line_color ^= opaque darkgreen) :
  (plot_lines_style .> line_color ^= opaque darkgrey) :
  (plot_lines_style .> line_color ^= opaque darkviolet) :
  colourisePlotLines

-- | Colourise the plot areas that are filling between 
-- any two lines.
colourisePlotFillBetween :: [PlotFillBetween x y -> PlotFillBetween x y]
colourisePlotFillBetween =
  (plot_fillbetween_style ^= solidFillStyle (withOpacity blue 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity green 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity red 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity black 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity grey 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity purple 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity violet 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity darkblue 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity darkgreen 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity darkgrey 0.4)) :
  (plot_fillbetween_style ^= solidFillStyle (withOpacity darkviolet 0.4)) :
  colourisePlotFillBetween