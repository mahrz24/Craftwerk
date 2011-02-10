module Main where

import Craftwerk.Core
import Craftwerk.UI

main = do displayRender . renderFigure
            figureToRenderContext $ Line (rectangle (0,0) (50,50))