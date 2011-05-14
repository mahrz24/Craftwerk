module Main where

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI
import Graphics.Craftwerk.Core.Driver.Cairo

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Writer
import Data.IORef

main = 
  do c1 <- choice "Color" ["red","green","blue"]
     c2 <- choice "Color" ["red","green","blue"]     
     let (f,ui) = runWriter (do col1 <- c1
                                col2 <- c2
                                return (do col <- readIORef col1
                                           return $ colorStyle col $ iterations 4 $ triangle))

     displayRender ui $ renderFigure 1 1 f
                        


-- displayRender
--        ([ ("Iteration", RangeOption 1 6 1 1)
--         , ("Color", ChoiceOption ["red"
--                                  ,"green"
--                                  ,"blue"] 0)])
--        $
--        renderFigure 1 1 (return $ colorStyle (choice $ opt Map.! "Color") $
--                                             iterations
--                                             (round $ value (opt Map.! "Iteration"))
--                                             triangle
--                         )
       
       
triangle = line [(0,0),(0,1),(1,0)]

colorStyle col = style newStyle { closePath = yes
                                , fillColor =
                                  Just $ case col of
                                              "red" -> red
                                              "green" -> green
                                              "blue" -> blue
                                , stroke = no
                                , fill = yes}

iterations :: Int -> Figure -> Figure
iterations 0 f = f
iterations i f =
  let nf = iterations (i-1) f
  in scale (0.5,0.5) $ composition
     [
       translate (0,1) $ nf
     , translate (0,0) $ nf
     , translate (1,0) $ nf
     ]

