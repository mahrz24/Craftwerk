module Main where

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI
import Graphics.Craftwerk.Core.Driver.Cairo

import qualified Data.Map as Map

main = do displayMultiple ([ ("Iteration",
                              RangeOption 1 6 1 1)
                           , ("Color", ChoiceOption ["red"
                                                    ,"green"
                                                    ,"blue"] 0)])
            $
            [ ("Iterator", renderFigure 1 1
                           (\opt -> return $
                                    style newStyle { closePath = yes
                                                   , fillColor =
                                                     Just $ [red,green,blue] !!
                                                     choice (opt Map.! "Color")
                                                   , stroke = no
                                                   , fill = yes} $
                                    iterations
                                    (round $ value (opt Map.! "Iteration"))
                                    (line [(0,0),(0,1),(1,0)])
                           ))]

iterations :: Int -> Figure -> Figure
iterations 0 f = f
iterations i f =
  let nf = iterations (i-1) f
  in scale (0.5,0.5) $ composition
     [
       translate (0,1) $ nf
     , translate (0,0) $ nf
     , translate (1,0) $ nf
     ]

