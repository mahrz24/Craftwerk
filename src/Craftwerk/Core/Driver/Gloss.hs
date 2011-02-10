module Craftwerk.Core.Driver.Gloss where
       
import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import qualified Graphics.Gloss.Data.Picture as Gloss
import qualified Graphics.Gloss.Data.Color as GlossColor

import Data.Maybe


figureToGlossPicture :: Figure -> Gloss.Picture
figureToGlossPicture = figureToGlossPictureWithStyle [Stroke True]

figureToGlossPictureWithStyle :: PList -> Figure -> Gloss.Picture 
figureToGlossPictureWithStyle _ Blank = Gloss.Blank
figureToGlossPictureWithStyle s (Style ns a) = (figureToGlossPictureWithStyle (mergeProperties s ns) a)
figureToGlossPictureWithStyle s (Rotate r a) = Gloss.Rotate r (figureToGlossPictureWithStyle s a)
figureToGlossPictureWithStyle s (Scale (x,y) a) = Gloss.Scale x y (figureToGlossPictureWithStyle s a)
figureToGlossPictureWithStyle s (Translate (x,y) a) = Gloss.Translate x y (figureToGlossPictureWithStyle s a)
figureToGlossPictureWithStyle s (Composition a) = Gloss.Pictures $ map (figureToGlossPictureWithStyle s) a 
figureToGlossPictureWithStyle s (Line a) = Gloss.Pictures $ (pathToStyledPolygon s a) ++ (pathToStyledLine s a)  
figureToGlossPictureWithStyle s (Text a) = Gloss.Text a

pathToStyledLine :: PList -> Path -> [Gloss.Picture]
pathToStyledLine s p = if stroke s then 
                         case lineColor s of
                           Just (RGBA r g b a) -> [Gloss.Color (GlossColor.makeColor r g b a) $ Gloss.Line (styledPath s p)] -- Needs to be changed indirect access
                           Nothing -> [Gloss.Line (styledPath s p)]
                       else []
                            
pathToStyledPolygon :: PList -> Path -> [Gloss.Picture]
pathToStyledPolygon s p = if fill s then 
                            case fillColor s of
                              Just (RGBA r g b a) -> [Gloss.Color (GlossColor.makeColor r g b a) $ Gloss.Polygon p]
                              Nothing -> [Gloss.Polygon p]
                       else []
                            
styledPath :: PList -> Path -> Path
styledPath s p = if closePath s then
                   p ++ [head p]
                 else p