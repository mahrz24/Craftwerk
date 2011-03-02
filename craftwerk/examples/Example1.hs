module Main where

import Craftwerk.Core

main = let w = 5
           h = 5
       in do putStrLn . figureToTikzPicture
          $ style defaultStyle { lineWidth = Just 1.5
                                 , closePath = yes
                                 }
          $ composition
          [
            line (rectangle (0.1,0.1) (w/2-0.1,h-0.2))
          , style emptyStyle { fill = yes
                             , fillColor = Just red
                             , dashes = Just [4.0,2.0,1.0,3.0]
                             }
            $ line (rectangle (w/2+0.1,0.1) (w/2-0.1,h-0.2))
          , style emptyStyle { lineWidth = Just 10.0
                             , lineCap   = Just CapRound
                             , lineJoin  = Just JoinMiter
                             , closePath = no
                             }
            $ composition [
                 line [(1,0.5),(2,1.3),(3,0.5)]
               , line [(1.9,1.7),(2,3.5),(2.1,1.7)]
               ]
          ]