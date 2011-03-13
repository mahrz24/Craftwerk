module Main where

import Graphics.Craftwerk.Core

-- A simple example that displays lines of differend widths
main = putStrLn . figureToTikzPicture
       $ style newStyle { closePath = no
                        , stroke = yes }
       $ composition
       [
         line [(5,0),(5,10)]
       , style (setLineWidth verythin) $ line [(1,1),(9,1)]
       , style (setLineWidth thin) $ line [(1,2),(9,2)]
       , style (setLineWidth semithick) $ line [(1,3),(9,3)]
       , style (setLineWidth thick) $ line [(1,4),(9,4)]
       , style newStyle { arrowTips = arrow (<=>)} $
         translate (0,4) $ composition
         [
           style (setLineWidth verythin) $ line [(1,1),(9,1)]
         , style (setLineWidth thin) $ line [(1,2),(9,2)]
         , style (setLineWidth semithick) $ line [(1,3),(9,3)]
         , style (setLineWidth thick) $ line [(1,4),(9,4)]
         ]
       ]

