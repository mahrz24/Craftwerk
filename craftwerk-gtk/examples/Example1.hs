module Main where

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI
import Graphics.Craftwerk.Core.Driver.Cairo

import qualified Data.Map as Map

main = do displayMultiple ([ ("Test", NumberOption 1)
                           , ("Test 1", 
                              RangeOption 0.0 10.0 1.0 5.0)
                           , ("Test 2", BoolOption False)
                           , ("Choice", ChoiceOption ["Hallo", 
                                                      "World"] 0)]) 
            $
            [ ("First", renderFigure 10 10 (\opt -> return $
                        style newStyle { closePath = no
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
                        ]))
            , ("Second", renderFigure 10 10 (\opt -> return $
                         style newStyle { lineWidth = Just (value $ opt Map.! "Test 1")
                                          , closePath =  Just (isSet $ opt Map.! "Test 2")
                                          , fillColor = rgb 0.4 0.2 (fromIntegral $ choice $ opt Map.! "Choice")
                                          , fill = yes }
                         $ composition
                         [
                           scale (10,10) $ line unitRectangle
                         , style newStyle { clip = yes } $ Circle (5,5) 5
                         , style newStyle { closePath = no, fill=no, arrowTips = arrow (<=>) } 
                           $ path [MoveTo (3,2),  CurveSegment (4,4) (1,5) (5,1)]
                         , style newStyle { closePath = no, fill=no, arrowTips = arrow (<=>) } 
                           $ path [MoveTo (3,5),  LineSegment (4,5), LineSegment (4.5,6)]
                         , style newStyle { closePath = no, fill=no, lineWidth = Just 1,  arrowTips = arrow (<=>) } 
                           $ path [MoveTo (1,5),  LineSegment (2,5), LineSegment (2.5,6)]
                         , path [MoveTo (6,2),  ArcSegment (6,2) 160 45 2]
                         , path [MoveTo (6,2),  ArcSegment (6,2) 45 160 2]
                         , Circle (0,0) 1
                         , style newStyle { lineWidth = Just $ 1, fill = no } $ Grid (5,5) 0.5 0.5
                         , canvas . (scale (1.5,2)) $ 
                           canvas . (rotate 20) $ 
                           canvas . (translate (3,1)) $ 
                           composition 
                           [
                             translate (2,0) $ rotate 45 $ style newStyle {fillColor = Just red} $ line unitRectangle
                           , scale (2,2) $ translate (0.5,0.5) $ style newStyle {fillColor = Just blue} $ line unitRectangle
                           , translate (0,2) $ canvas . (scale (2,2)) $ style newStyle {fillColor = Just green} $ line unitRectangle
                             
                           ]
                         ])) ]
          
           
                       
                    --    [ style newStyle { lineWidth = Just 1.5
                    --                  , closePath = yes
                    --                  , fillColor = Just (makeColor 0.2 0.1 0.5)
                    --                  }
                    --   $ composition [ line (rectangle (0.1,0.1) (w/2-0.1,h-0.2))
                    --                 , style newStyle { fill = yes
                    --                                  , fillColor = Just red
                    --                                  , dashes = Just [4.0,2.0,1.0,3.0]
                    --                                  }
                    --                   $ line (rectangle (w/2+0.1,0.1) (w/2-0.2,h-0.2))
                    --                 , style newStyle { fill = yes } $ line (rectangle (w/3+0.1,0.1) (w/3-0.1,h-0.2))
                    --                 , style newStyle { lineWidth = Just 10.0
                    --                                  , lineCap   = Just CapRound
                    --                                  , lineJoin  = Just JoinMiter
                    --                                  , closePath = no
                    --                                  }
                    --                   $ composition [
                    --                      line [(1,0.5),(2,1.3),(3,0.5)]
                    --                      , line [(1.9,1.7),(2,3.5),(2.1,1.7)]
                    --                      ]
                    --                 ]
                    -- ]