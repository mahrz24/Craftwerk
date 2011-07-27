module Main where

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI
import Graphics.Craftwerk.Core.Driver.Cairo


import Data.Complex

main = runGTVInWindow "Example 3" figureEx3

figureEx3 :: GTV (Figure)
figureEx3 = tv (figureOut 10.0 10.0) $
              style newStyle { lineWidth = width 2.0
                             , closePath = yes
                             , fillColor = Just beige
                             , fill = yes } $
              composition
              [
                scale (10 :+ 10) $ line unitRectangle
              -- , style newStyle { clip = yes } $ Circle (5,5) 5
              , Circle origin 5
              , style newStyle { closePath = no, fill=no, arrowTips = arrow (<=>) }
                $ path [MoveTo (3 :+ 2),  CurveSegment (4 :+ 4) (1 :+ 5) (5 :+ 1)]
              , style newStyle { closePath = no, fill=no, arrowTips = arrow (<=>) }
                $ path [MoveTo (3 :+ 5),  LineSegment (4 :+ 5), LineSegment (4.5 :+ 6)]
              , rotate 10 $ style newStyle { closePath = no
                                           , fill=no
                                           , lineWidth = Just 1
                                           , arrowTips = arrow (<=>) }
                $ path [MoveTo (1 :+ 5),  LineSegment (2 :+ 5), LineSegment (2.5 :+ 6)]
              , canvas . (scale (1.2 :+ 1.2)) $ path [MoveTo (6 :+ 2),  ArcSegment (6 :+ 2) 160 45 2]
              , path [MoveTo (6 :+ 2),  ArcSegment (6 :+ 2) 45 160 2]
              , style newStyle { fill = yes, fillColor = Just red, dashes = Just [0.5 , 0.5] } $
                translate (5 :+ 3) $ scale (0.2 :+ 0.2) $ text "Hallo World"
              , style newStyle { lineWidth = Just $ 1, fill = no }
           $ Grid (10 :+ 10) 1.0 1.0
              , canvas . (scale (1.5 :+ 2)) $
                canvas . (rotate 20) $
                canvas . (translate (3 :+ 0.5)) $
                composition
                [
                  translate (2 :+ 0) $ rotate 45 $
                  style newStyle {fillColor = Just red} $ line unitRectangle
                , scale (2 :+ 2) $ translate (0.5 :+ 0.5) $
                  style newStyle {fillColor = Just blue} $ line unitRectangle
                , translate (0 :+ 2) $ canvas . (scale (2 :+ 2)) $
                  style newStyle {fillColor = Just green} $ line unitRectangle

                ]
              ]


