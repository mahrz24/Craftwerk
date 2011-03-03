-- |
-- Module      :  Craftwerk.Core.Style
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.Core.Driver.Tikz (
  -- * TikZ conversion
  figureToTikzPicture
  ) where

import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import Data.Maybe
import Data.List
import Text.Printf

import Control.Monad
import Control.Monad.Reader

data Context = Context { styleP :: StyleProperties }

-- | Convert a Craftwerk 'Figure' to a TikZ picture environment string
figureToTikzPicture :: Figure -> String
figureToTikzPicture f = 
  environment "tikzpicture" [] (runReader (figureToTikzPictureWithStyle f) $
  Context { styleP = defaultStyle })
  
figureToTikzPictureWithStyle :: Figure -> Reader Context String
figureToTikzPictureWithStyle Blank = return ""
figureToTikzPictureWithStyle (Style ns a) =
  local (\c -> c { styleP = mergeProperties (styleP c) ns}) $
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Transform (Rotate r) a) = do
  figure <- (figureToTikzPictureWithStyle a)
  return $ scope (numArgumentList [("rotate",r,"")]) figure
  

figureToTikzPictureWithStyle (Transform (Scale (x,y)) a) = do
  figure <- (figureToTikzPictureWithStyle a)
  return $ scope (numArgumentList [("xscale",x,"cm"),("yscale",y,"cm")]) figure

figureToTikzPictureWithStyle (Transform (Translate (x,y)) a) = do
  figure <- (figureToTikzPictureWithStyle a)
  return $ scope (numArgumentList [("xshift",x,"cm"),("yshift",y,"cm")]) figure

figureToTikzPictureWithStyle (Composition a) =
   concat `liftM` mapM figureToTikzPictureWithStyle a

figureToTikzPictureWithStyle (Text a) = return $ node a

figureToTikzPictureWithStyle (Line a) =  ask >>= \c ->
  let sp = getProperty (styleP c)
  in return $ (xcolor "linec" $ sp lineColor)
      ++ (xcolor "fillc" $ sp fillColor)
      ++ (lineCommand sp 
          (dashProperties (sp dashPhase) (sp dashes))
          (lineStyle (sp lineCap) (sp lineJoin) (sp miterLimit))
         )
      ++ "["
          ++ (lineColorDesc sp)
          ++ ",line width="
          ++ (printNum $ sp lineWidth)
          ++ "] "
      ++ intercalate " -- "
          (map (\(x,y) -> "(" ++ (printNum x) ++ "," ++ (printNum y) ++ ")") a)
      ++ (if (sp closePath) then " -- cycle" else "")
      ++ ";\n"

lineColorDesc sp = if (sp fill) && (sp stroke) then
                  "fill=fillc,draw=linec"
                else
                  if (sp fill) then
                    "color=fillc"
                    else
                    "color=linec"
                    

lineCommand sp dp ls  = if (sp fill) && (sp stroke) then
                  "\\filldraw[" ++ dp ++ "," ++ ls ++ "]"
                else
                  if (sp fill) then
                    "\\fill"
                    else
                    "\\draw[" ++ dp ++ "," ++ ls ++ "]"

lineStyle lc lj ml = 
  "line cap=" ++ 
  (case lc of
    CapRect -> "rect"
    CapButt -> "butt"
    CapRound -> "round")
  ++ ", line join=" ++
  (case lj of
    JoinRound -> "round"
    JoinBevel -> "bevel"
    JoinMiter -> "miter")
  ++ ", miter limit=" ++ (printNum ml) 

dashProperties phase pattern = if (length pattern) > 0 then
                      "dash phase=" ++ (printNum phase) ++ 
                      ",dash pattern=" ++ (dashPattern True pattern)  ++""
                    else
                      "solid"
                      
dashPattern :: Bool -> [Float] -> String
dashPattern b (x:xs) = (if b then "on " else "off ") ++ (printNum x) ++ " " 
                       ++ dashPattern (not b) xs
dashPattern _ _ = ""

-- * TikZ/PGF & xcolor Commands

xcolor name (RGBA r g b a) = 
  texCommand "definecolor" 
  [ name
  , "rgb"
  , (printf "%.2f,%.2f,%.2f" r g b)]
                             
scope args body = environment "scope" args body

node n = texCommand "node" [n]

-- * TeX Output

printNum n = printf "%.4f" n

argumentList argList = 
  map (\(l,n) -> l ++ "=" ++ n) argList
  
numArgumentList argList = 
  map (\(l,n,u) -> l ++ "=" ++ (printNum n) ++ u) argList

tikzArguments :: [String] -> String
tikzArguments [] = ""
tikzArguments args = "[" ++ (intercalate "," args) ++ "]"

texArguments :: [String] -> String
texArguments args = concatMap (\s -> "{" ++ s ++ "}") args


texCommand cmd args = 
  "\\" ++ cmd ++ (texArguments args) ++ "\n"

tikzCommand cmd args body = 
  "\\" ++ cmd ++ (tikzArguments args) 
  ++ " " ++ body ++ ";\n"

environment env args body = 
  "\\begin{" ++ env ++ "}" ++ (tikzArguments args) ++ "\n" 
  ++ body 
  ++ "\\end{" ++ env ++ "}\n"


