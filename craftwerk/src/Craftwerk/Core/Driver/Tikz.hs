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

-- | Convert a Craftwerk 'Figure' to a TikZ picture environment string
figureToTikzPicture :: Figure -> String
figureToTikzPicture f = "\\begin{tikzpicture}\n" ++
                        (figureToTikzPictureWithStyle defaultStyle f) ++
                        "\\end{tikzpicture}\n"

figureToTikzPictureWithStyle :: StyleProperties -> Figure -> String
figureToTikzPictureWithStyle _ Blank = ""
figureToTikzPictureWithStyle s (Style ns a) =
  (figureToTikzPictureWithStyle (mergeProperties s ns) a)

figureToTikzPictureWithStyle s (Transform (Rotate r) a) =
  scope ("rotate=" ++ (printNum r))
  (figureToTikzPictureWithStyle s a)

figureToTikzPictureWithStyle s (Transform (Scale (x,y)) a) =
  scope ("xscale=" ++ (printNum x) ++ "cm, yscale=" ++ (printNum y) ++ "cm")
  (figureToTikzPictureWithStyle s a)

figureToTikzPictureWithStyle s (Transform (Translate (x,y)) a) =
  scope ("xshift=" ++ (printNum x) ++ "cm, yshift=" ++ (printNum y) ++ "cm")
  (figureToTikzPictureWithStyle s a)

figureToTikzPictureWithStyle s (Composition a) =
  concatMap (figureToTikzPictureWithStyle s) a

figureToTikzPictureWithStyle s (Text a) = "\node {" ++ a ++ "}\n"

figureToTikzPictureWithStyle s (Line a) =
  let sp = getProperty s
  in  (xcolor "linec" $ sp lineColor)
      ++ (xcolor "fillc" $ sp fillColor)
      ++ (lineCommand sp (sp dashPhase) (sp dashes))
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

lineCommand sp phase pattern = if (sp fill) && (sp stroke) then
                  "\\filldraw" ++ (dashProperties phase pattern)
                else
                  if (sp fill) then
                    "\\fill"
                    else
                    "\\draw" ++ (dashProperties phase pattern)

dashProperties phase pattern = if (length pattern) > 0 then
                      "[dash phase=" ++ (printNum phase) ++ 
                      ",dash pattern=" ++ (dashPattern True pattern)  ++"]"
                    else
                      ""
                      
dashPattern :: Bool -> [Float] -> String
dashPattern b (x:xs) = (if b then "on " else "off ") ++ (printNum x) ++ " " 
                       ++ dashPattern (not b) xs
dashPattern _ _ = ""

xcolor name (RGBA r g b a) =
  "\\definecolor{"
  ++ name
  ++ "}{rgb}{"
  ++ (printf "%.2f,%.2f,%.2f" r g b)
  ++ "}\n"

scope prop body = "\\begin{scope}[" ++ prop ++ "]\n" ++ body ++ "\\end{scope}\n"

printNum n = printf "%.4f" n
