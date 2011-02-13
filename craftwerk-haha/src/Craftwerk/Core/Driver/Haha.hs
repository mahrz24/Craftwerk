-- |
-- Module      :  Craftwerk.Core.Driver.Haha
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.Core.Driver.Haha (
  -- * Haha conversion
  figureToHahaString
  ) where

import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import Data.Maybe
import Data.List
import Text.Printf

import Graphics.Ascii.Haha.Plot
import qualified Graphics.Ascii.Haha.Bitmap as Haha
import qualified Graphics.Ascii.Haha.Geometry as G
import Graphics.Ascii.Haha.Terminal

screen :: G.Rect Integer
screen = G.Rect (G.Point 0 0) (G.Point 80 40)

view :: G.Rect Float
view = G.Rect (G.Point 0 0) (G.Point 80 40)

type Bmp = Haha.Bitmap Float Pixel

-- | Convert a Craftwerk 'Figure' to a TikZ picture environment string
figureToHahaString :: Figure -> String
figureToHahaString f = 
  string True screen (G.Point 0 0) " " "" . list 1 view $
    foldr applyF Haha.empty $ (figureToHahaStringWithStyle 
     defaultStyle 
     (((1.0,0.0),(0.0,1.0)),(0.0,0.0))
     f)
  where applyF f b = f b

vmul ((a,b),(c,d)) (e,f) = (a*e+b*f,c*e+d*f)
mmul ((a,b),(c,d)) ((e,f),(g,h)) = ((a*e+b*g,a*f+b*h),(c*e+d*g,c*f+d*h))
rot theta = ((cos theta, -sin theta),(sin theta, cos theta))
scl (a,b) = ((a,0),(0,b))
addv (a,b) (c,d) = (a+c,b+d)
app (m,t) v =  addv t $ vmul m v 

figureToHahaStringWithStyle :: StyleProperties 
                                -> ((Vector,Vector),Vector)
                                -> Figure 
                                -> [(Bmp -> Bmp)]
                                
figureToHahaStringWithStyle _ _ Blank = []

figureToHahaStringWithStyle s v (Style ns a) =
  (figureToHahaStringWithStyle (mergeProperties s ns) v a)

figureToHahaStringWithStyle s (m,t) (Transform (Rotate r) a) =
  let rm = rot r
  in (figureToHahaStringWithStyle s (mmul rm m, t) a)

figureToHahaStringWithStyle s (m,t) (Transform (Scale v) a) =
  let sm = scl v
  in (figureToHahaStringWithStyle s (mmul m sm, t) a)

figureToHahaStringWithStyle s (m,t) (Transform (Translate v) a) =
  (figureToHahaStringWithStyle s (m, addv t (vmul m v) ) a)

figureToHahaStringWithStyle s v (Composition a) =
  concatMap (figureToHahaStringWithStyle s v) a

figureToHahaStringWithStyle s v (Text a) = []

figureToHahaStringWithStyle s v (Line a) =
  let sp = getProperty s
      b = app v $ head a
      e = app v $ last a
  in [Haha.drawPoly (G.Poly (map ((\(x,y) -> G.Point x y) . (app v))  a)) $ Pixel 'x' black] ++
  (if (sp closePath) then 
     [Haha.drawLine 
      (G.Line (G.Point (fst b) $  (snd b)) (G.Point (fst e) $ (snd e))) $ Pixel 'x' black] 
   else [])