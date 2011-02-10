module Main where

import Craftwerk.Core
import Craftwerk.UI

main = do displayRender renderFigure
           where renderFigure w h = figureToRenderContext 
                                    $ Style defaultStyle { lineWidth = Just 2.0, closePath = yes } $ Composition 
                                    [ Line (rectangle (5,5) (w/2-5,h-10))
                                    , Style emptyStyle { fill = yes, fillColor = Just red} $ Line (rectangle (w/2+5,5) (w/2-10,h-10))]