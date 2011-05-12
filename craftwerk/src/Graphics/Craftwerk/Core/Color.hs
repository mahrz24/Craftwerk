-- |
-- Module      :  Graphics.Craftwerk.Core.Color
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- Craftwerk uses the "Data.Colour" package to represent colours in styles.

module Graphics.Craftwerk.Core.Color
       ( module Data.Colour.SRGB
       , module Data.Colour.Names

       , FigureColor

       , integralColor
  ) where

import Data.Colour.SRGB
import Data.Colour.Names


-- | The colour type used by Craftwerk
type FigureColor = Colour Double

-- | Returns the i-th of 16 basic named colours (i % 16 to be precise).
integralColor :: Integral a => a -> FigureColor
integralColor i = concat (repeat stdColors) !! fromIntegral i

stdColors = [ white
            , silver
            , gray
            , black
            , red
            , maroon
            , yellow
            , olive
            , lime
            , green
            , aqua
            , teal
            , blue
            , navy
            , fuchsia
            , purple] :: [FigureColor]


