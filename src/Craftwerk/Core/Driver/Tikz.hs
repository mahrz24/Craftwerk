module Craftwerk.Core.Driver.Tikz where
       
import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import Data.Maybe
import Data.List
import Text.Printf

figureToTikzPicture :: Figure -> String
figureToTikzPicture f = "\\begin{tikzpicture}\n" ++
                        (figureToTikzPictureWithStyle [Stroke True] f) ++
                        "\\end{tikzpicture}\n"

figureToTikzPictureWithStyle :: PList -> Figure -> String
figureToTikzPictureWithStyle _ Blank = ""
figureToTikzPictureWithStyle s (Style ns a) = (figureToTikzPictureWithStyle (mergeProperties s ns) a)
figureToTikzPictureWithStyle s (Rotate r a) = scope ("rotate=" ++ (printNum r)) (figureToTikzPictureWithStyle s a)
figureToTikzPictureWithStyle s (Scale (x,y) a) = scope ("xscale=" ++ (printNum x) ++ "cm, yscale=" ++ (printNum y) ++ "cm") (figureToTikzPictureWithStyle s a)
figureToTikzPictureWithStyle s (Translate (x,y) a) = scope ("xshift=" ++ (printNum x) ++ "cm, yshift=" ++ (printNum y) ++ "cm") (figureToTikzPictureWithStyle s a)
figureToTikzPictureWithStyle s (Composition a) = concatMap (figureToTikzPictureWithStyle s) a 
figureToTikzPictureWithStyle s (Text a) = "\node {" ++ a ++ "}\n"
figureToTikzPictureWithStyle s (Line a) = (xcolor "linec" $ maybe (makeColor 0 0 0) id $ lineColor s) 
                                          ++ (xcolor "fillc" $ maybe (makeColor 1 1 1) id $ fillColor s) 
                                          ++ (lineCommand s) 
                                          ++ "[" ++ (lineColorDesc s) ++ ",line width=" ++ (printNum $ lineWidth s) ++ "] "
                                          ++ intercalate " -- " (map (\(x,y) -> "(" ++ (printNum x) ++ "," ++ (printNum y) ++ ")") a) 
                                          ++ (if (closePath s) then " -- cycle" else "")
                                          ++ ";\n"

lineColorDesc s = if (fill s) && (stroke s) then 
                  "fill=fillc,draw=linec"
                else
                  if fill s then
                    "color=fillc"
                    else
                    "color=linec"
                    
lineCommand s = if (fill s) && (stroke s) then 
                  "\\filldraw"
                else
                  if fill s then
                    "\\fill"
                    else
                    "\\draw"
                    
xcolor name (RGBA r g b a) = "\\definecolor{" ++ name ++ "}{rgb}{" ++ (printf "%.2f,%.2f,%.2f" r g b) ++ "}\n"

scope prop body = "\\begin{scope}[" ++ prop ++ "]\n" ++ body ++ "\\end{scope}\n"

printNum n = printf "%.4f" n