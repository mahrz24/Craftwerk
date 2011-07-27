module Main where

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI
import Graphics.Craftwerk.Core.Driver.Cairo

import Data.Complex

main = runGTVInWindow "Test" pinski

in1 = sliderIIn (0,10) 1

pinski :: GTV (Colour Double -> Int -> Figure)
pinski = tv
         (oTitle "Sierpinski Triangle" $
          oLambda (iTitle "Color" defaultIn) (oLambda (iTitle "Iterations" in1) (figureOut 2.0 1.0)) ) tpair

tpair col i = colorStyle col $ iterations i $ triangle

colorStyle col = style newStyle { closePath = yes
                                , fillColor = Just col
                                , stroke = no
                                , fill = yes}

triangle = line [(0 :+ 0),(0 :+ 1),(1 :+ 0)]

iterations :: Int -> Figure -> Figure
iterations 0 f = f
iterations i f =
  let nf = iterations (i-1) f
  in scale (0.5 :+ 0.5) $ composition
     [
       translate (0 :+ 1) $ nf
     , translate (0 :+ 0) $ nf
     , translate (1 :+ 0) $ nf
     ]
