module Main where

import Craftwerk.Core

main = let w = 10
           h = 10
       in do putStrLn . figureToTikzPicture
             $ style newStyle { lineWidth = Just $ 3*1.5 
                              , closePath = yes
                              , fillColor = Just $ makeColor 0.4 0.2 0.8
                              , fill = yes }
             $ composition
               [
                 scale (10,10) $ line unitRectangle
               , canvas . (scale (1.5,2)) $ 
                 canvas . (rotate 20) $ 
                 canvas . (translate (3,1)) $ 
                 composition 
                 [
                   translate (2,0) $ rotate 45 $ style newStyle {fillColor = Just red} $ line unitRectangle
                 , scale (2,2) $ translate (0.5,0.5) $ style newStyle {fillColor = Just blue} $ line unitRectangle
                 , translate (0,2) $ canvas . (scale (2,2)) $ style newStyle {fillColor = Just green} $ line unitRectangle
                 ]
               ]
             
             
          -- $ composition 
          -- [ line (rectangle (0.1,0.1) (w/2-0.1,h-0.2)),
          --   style newStyle { lineWidth = Just 3
          --                    , closePath = yes
          --                    , fillColor = Just (makeColor 0.2 0.1 0.5)
          --                    }
          --   $ composition
          --   [
          --     line (rectangle (0.1,0.1) (w/2-0.1,h-0.2))
          --   , style newStyle { fill = yes
          --                      , fillColor = Just red
          --                      , dashes = Just [4.0,2.0,1.0,3.0]
          --                      }
          --     $ line (rectangle (w/2+0.1,0.1) (w/2-0.2,h-0.2))
          --   , style newStyle { fill = yes } $ line (rectangle (w/3+0.1,0.1) (w/3-0.1,h-0.2))
          --   , style newStyle { lineWidth = Just 10.0
          --                      , lineCap   = Just CapRound
          --                      , lineJoin  = Just JoinMiter
          --                      , closePath = no
          --                      }
          --     $ composition [
          --        line [(1,0.5),(2,1.3),(3,0.5)]
          --        , line [(1.9,1.7),(2,3.5),(2.1,1.7)]
          --        ]
          --   ]
          -- ]]