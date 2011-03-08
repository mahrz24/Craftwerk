module Main where

import Craftwerk.Core

main = let w = 10
           h = 10
       in do putStrLn . figureToTikzPicture
             $ style newStyle { closePath = no
                              , fillColor = rgb 0.4 0.2 0.8
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
               -- composition
               -- [
               --   scale (10,10) $ line unitRectangle
               -- , style newStyle { clip = yes } $ circle (5,5) 5
               -- , style newStyle { closePath = no, fill=no, arrowTips = arrow (<=>) } 
               --   $ path [moveTo (3,2),  curveTo (4,4) (1,5) (5,1)]
               -- , style newStyle { closePath = no, fill=no, arrowTips = arrow (<==) } 
               --   $ path [moveTo (3,5),  lineTo (6,6)]
               -- , path [moveTo (6,2),  arc (6,2) 160 45 2]
               -- , path [moveTo (6,2),  arc (6,2) 45 160 2]
               -- , circle (0,0) 1
               -- , style newStyle { lineWidth = width 1, fill = no } $ grid (5,5) (0.5, 0.5)
               -- , canvas . (scale (1.5,2)) $ 
               --   canvas . (rotate 20) $ 
               --   canvas . (translate (3,1)) $ 
               --   composition 
               --   [
               --     translate (2,0) $ rotate 45 $ style newStyle {fillColor = Just red} $ line unitRectangle
               --   , scale (2,2) $ translate (0.5,0.5) $ style newStyle {fillColor = Just blue} $ line unitRectangle
               --   , translate (0,2) $ canvas . (scale (2,2)) $ style newStyle {fillColor = Just green} $ line unitRectangle
                 
               --   ]
               -- ]
             
             
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