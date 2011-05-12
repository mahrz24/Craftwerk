module Main where

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI
import Graphics.Craftwerk.Core.Driver.Cairo

import Text.Printf

import qualified Data.Map as Map

copyOption :: Integer -> [(String, Option)]
copyOption n = 
  let s = printf " %i" n
  in [ ("Show"++s, BoolOption True)
     , ("Rotation"++s, RangeOption 0 360 1 0)
     , ("Shift x"++s, RangeOption 0 1.5 0.1 0)
     , ("Shift y"++s, RangeOption 0 1.5 0.1 0)
     , ("Scale x"++s, RangeOption 0 2 0.1 0.5)
     , ("Scale y"++s, RangeOption 0 2 0.1 0.5)
     ]
     
copyValue :: String -> Map.Map String Option ->  Integer  -> Double
copyValue name opt n =
  let s = printf " %i" n
  in value (opt Map.! (name ++ s))

copies = [1..4]

rotation = copyValue "Rotation"
scaling opt n = (copyValue "Scale x" opt n, copyValue "Scale y" opt n)
translation opt n = (copyValue "Shift x" opt n, copyValue "Shift y" opt n)
showOption opt n =
  let s = printf " %i" n
  in isSet (opt Map.! ("Show" ++ s))

main = 
  do displayRender 
       ([ ("Iteration", RangeOption 1 7 1 1)
        , ("Outline", BoolOption True)
        , ("Color", ChoiceOption ["red"
                                 ,"green"
                                 ,"blue"] 0)] ++
       (concatMap copyOption copies))
       $ renderFigure 1 1 (\opt -> return $
                                   colorStyle opt $
                                   translate (0.15,0.15) $
                                   scale (0.7,0.7) $
                                   iterations opt
                                   (round $ value (opt Map.! "Iteration"))
                                   (line unitRectangle)
                          )

colorStyle opt = 
  let color = Just $ [red,green,blue] !! (choice $ opt Map.! "Color")
      outline = (isSet $ opt Map.! "Outline")
  in style newStyle { closePath = yes
                    , fillColor = color
                    , lineColor = color
                    , lineWidth = width 0.5
                    , stroke = Just outline
                    , fill = Just $ not outline}

iterations :: Map.Map String Option -> Integer -> Figure -> Figure
iterations _ 0 f = f
iterations opt i f =
  let nf = iterations opt (i-1) f
  in composition $ map (\i -> translate (translation opt i) $
                              scale (scaling opt i) $
                              rotate (rotation opt i) $ nf) $
     filter (\i -> showOption opt i) copies

