module Main where

import Craftwerk.Core

main = let w = 5
           h = 5
       in do putStrLn . figureToTikzPicture
          $ Style defaultStyle { lineWidth = Just 1.5
                                 , closePath = yes 
                                 } 
          $ Composition 
          [ 
            Line (rectangle (0.1,0.1) (w/2-0.1,h-0.2))
          , Style emptyStyle { fill = yes
                             , fillColor = Just red
                             } 
            $ Line (rectangle (w/2+0.1,0.1) (w/2-0.1,h-0.2))
          ]