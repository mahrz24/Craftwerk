module Main where

import Craftwerk.Core
import Craftwerk.UI
import Craftwerk.Core.Driver.Cairo

main = do displayRender renderFigure
            where renderFigure wx hx = 
                    let w = 5
                        h = 5
                    in figureToRenderContext $ scale (wx/w, -hx/h) $ translate (0,-h) $ composition 
                    [ style newStyle { lineWidth = Just 1.5
                                     , closePath = yes
                                     , fillColor = Just (makeColor 0.2 0.1 0.5)
                                     }
                      $ composition [ line (rectangle (0.1,0.1) (w/2-0.1,h-0.2))
                                    , style newStyle { fill = yes
                                                     , fillColor = Just red
                                                     , dashes = Just [4.0,2.0,1.0,3.0]
                                                     }
                                      $ line (rectangle (w/2+0.1,0.1) (w/2-0.2,h-0.2))
                                    , style newStyle { fill = yes } $ line (rectangle (w/3+0.1,0.1) (w/3-0.1,h-0.2))
                                    , style newStyle { lineWidth = Just 10.0
                                                     , lineCap   = Just CapRound
                                                     , lineJoin  = Just JoinMiter
                                                     , closePath = no
                                                     }
                                      $ composition [
                                         line [(1,0.5),(2,1.3),(3,0.5)]
                                         , line [(1.9,1.7),(2,3.5),(2.1,1.7)]
                                         ]
                                    ]
                    ]