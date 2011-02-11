-- |
-- Module      :  Craftwerk.Core.Driver.Gloss
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.Core.Driver.Gloss (
  -- * Gloss conversion
  figureToGlossPicture
  ) where

import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import qualified Graphics.Gloss.Data.Picture as Gloss
import qualified Graphics.Gloss.Data.Color as GlossColor

import Data.Maybe

-- | Convert a Craftwerk 'Figure' to a gloss Picture
figureToGlossPicture :: Figure -> Gloss.Picture
figureToGlossPicture = figureToGlossPictureWithStyle defaultStyle

figureToGlossPictureWithStyle :: StyleProperties -> Figure -> Gloss.Picture

figureToGlossPictureWithStyle _ Blank = Gloss.Blank

figureToGlossPictureWithStyle s (Style ns a) =
  (figureToGlossPictureWithStyle (mergeProperties s ns) a)

figureToGlossPictureWithStyle s (Rotate r a) =
  Gloss.Rotate r (figureToGlossPictureWithStyle s a)

figureToGlossPictureWithStyle s (Scale (x,y) a) =
  Gloss.Scale x y (figureToGlossPictureWithStyle s a)

figureToGlossPictureWithStyle s (Translate (x,y) a) =
  Gloss.Translate x y (figureToGlossPictureWithStyle s a)

figureToGlossPictureWithStyle s (Composition a) =
  Gloss.Pictures $ map (figureToGlossPictureWithStyle s) a

figureToGlossPictureWithStyle s (Line a) =
  Gloss.Pictures $ (pathToStyledPolygon s a) ++ (pathToStyledLine s a)

figureToGlossPictureWithStyle s (Text a) =
  Gloss.Text a

pathToStyledLine :: StyleProperties -> Path -> [Gloss.Picture]
pathToStyledLine s p = let sp = getProperty s
                       in if sp stroke  then
                            [Gloss.Color (glossColor (sp lineColor)) $
                             Gloss.Line (styledPath sp p)]
                          else []

pathToStyledPolygon :: StyleProperties -> Path -> [Gloss.Picture]
pathToStyledPolygon s p = let sp = getProperty s
                          in if sp fill then
                               [Gloss.Color (glossColor (sp fillColor)) $
                                Gloss.Polygon p]
                             else []

styledPath sp p = if sp closePath then
                   p ++ [head p]
                 else p

glossColor :: Color -> GlossColor.Color
glossColor (RGBA r g b a) = GlossColor.makeColor r g b a

