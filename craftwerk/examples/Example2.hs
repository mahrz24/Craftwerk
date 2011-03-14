module Main where

import Graphics.Craftwerk.Core

-- A simple example that displays the craftwerk logo
main = putStrLn . figureToTikzPicture
       $ style newStyle { closePath = yes
                        , fillColor = Just red
                        , lineWidth = width 2.0
                        , stroke = yes 
                        , fill = yes }
       $ composition
       [
         path 
         [
           moveTo (-0.02,0)
         , lineTo (1.1,0)
         , lineTo (1.1,0.35)
         , lineTo (1.07,0.5)
         , lineTo (1.06,0.8)
         , lineTo (0.86,0.8)
         , lineTo (0.85,0.5)
         , lineTo (0.65,0.35)
         , lineTo (0.65,0.5)
         , lineTo (0.45,0.35)
         , lineTo (0.45,0.5)
         , lineTo (0.25,0.35)
         , lineTo (0.25,0.5)
         , lineTo (0.05,0.35)
         , lineTo (-0.02,0.35)
         ]
       ]
