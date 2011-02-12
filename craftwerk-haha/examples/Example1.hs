module Main where

import Craftwerk.Core
import Craftwerk.Core.Driver.Haha

main = let w = 80
           h = 40
       in do 
         putStrLn . figureToHahaString
          $ Style defaultStyle { lineWidth = Just 1.5
                                 , closePath = yes
                                 }
          $ Composition
          [
            Line (rectangle (0,0) (w/2-2,h-2))
          ,  Scale (0.5,0.5) $ Translate (w,h/2) $ Rotate 0.2 $ Style emptyStyle { fill = yes
                             , fillColor = Just red
                             }
            $ Line (rectangle (0,0) (w/2-2,h-2))
          ]