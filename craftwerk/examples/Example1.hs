module Main where

import Graphics.Craftwerk.Core

-- A simple example that displays lines of differend widths
main = putStrLn . figureToTikzPicture
       $ style newStyle { closePath = no
                        , stroke = yes }
       $ composition
       [
         line [(3,0.5),(3,5)]
       , testlines
       , style newStyle { arrowTips = arrow (<=>)} $
         translate (0,0.5) $ testlines
       ]
       
testlines = 
  composition 
  [ style (setLineWidth verythin) $ line [(1,1),(5,1)]
  , style (setLineWidth thin) $ line [(1,2),(5,2)]
  , style (setLineWidth semithick) $ line [(1,3),(5,3)]
  , style (setLineWidth thick) $ line [(1,4),(5,4)]
  ]

