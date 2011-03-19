-- |
-- Module      :  Graphics.Craftwerk.Core.Driver.Tikz
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- Convert a figure to a LaTeX TikZ 
-- (<http://sourceforge.net/projects/pgf/develop>) string.

module Graphics.Craftwerk.Core.Driver.Tikz (
  -- * TikZ conversion
  figureToTikzPicture
  ) where

import Graphics.Craftwerk.Core.Figure
import Graphics.Craftwerk.Core.Color
import Graphics.Craftwerk.Core.Style

import Data.Maybe
import Data.List
import Text.Printf

import Control.Monad
import Control.Monad.Reader

data Context = Context { styleP :: StyleProperties 
                       , fillDepth :: Int
                       , strokeDepth :: Int
                       , coordinateMatrix :: String
                       , inverseCoordinateMatrix :: String
                       }

-- | Convert a Craftwerk 'Figure' to a TikZ picture environment string
figureToTikzPicture :: Figure -> String
figureToTikzPicture f =
  environment "tikzpicture" (styleArguments defaultStyle) $
  xcolor "linec" (getProperty defaultStyle lineColor) ++
  xcolor "fillc" (getProperty defaultStyle fillColor) ++
  runReader (figureToTikzPictureWithStyle f)
   Context { styleP = defaultStyle
           , fillDepth = 0
           , strokeDepth = 0
           , coordinateMatrix = ""
           , inverseCoordinateMatrix = ""}

figureToTikzPictureWithStyle :: Figure -> Reader Context String
figureToTikzPictureWithStyle Blank = return ""

figureToTikzPictureWithStyle (Style ns a) = 
  local (\c -> c { styleP = mergeProperties (styleP c) ns
                 , fillDepth = fillDepth c + maybeToInt (fillColor ns)
                 , strokeDepth = strokeDepth c + maybeToInt (lineColor ns)
                 }) (do c <- ask
                        prependColor (scopePrefix (strokeDepth c) ++ "linec") 
                          ns lineColor 
                          $ prependColor (scopePrefix (fillDepth c) ++ "fillc") 
                          ns fillColor 
                          $ liftM (scope $ styleArguments ns) 
                          (figureToTikzPictureWithStyle a))


figureToTikzPictureWithStyle (Canvas (Rotate r) a) = ask >>= \c ->
  liftM
  (emptyScope .
   (++)
   (coordinateMatrix c ++
    pgfLowLevel "rotate" (printNum r) ++
    inverseCoordinateMatrix c))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Canvas (Scale (x,y)) a) = ask >>= \c ->
  liftM
  (emptyScope .
   (++)
   (coordinateMatrix c ++
    pgfLowLevel "xscale" (printNum x) ++
    pgfLowLevel "yscale" (printNum y) ++
    inverseCoordinateMatrix c))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Canvas (Translate (x,y)) a) = ask >>= \c ->
  liftM
  (emptyScope .
   (++)
   (coordinateMatrix c ++
    pgfLowLevel "xshift" ((printNum x) ++ "cm") ++
    pgfLowLevel "yshift" ((printNum y) ++ "cm") ++
    inverseCoordinateMatrix c))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Transform (Rotate r) a) =
  local (\c -> c { coordinateMatrix = 
                      coordinateMatrix c ++ 
                      pgfLowLevel "rotate" (printNum r)
                 , inverseCoordinateMatrix =
                        pgfLowLevel "rotate" (printNum $ -r) ++
                        inverseCoordinateMatrix c 
                 }) $
  liftM (scope (numArgumentList [("rotate",r,"")]))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Transform (Scale (x,y)) a) =
    local (\c -> c { coordinateMatrix = 
                      coordinateMatrix c ++ 
                      pgfLowLevel "xscale" (printNum x) ++
                      pgfLowLevel "yscale" (printNum y)
                 , inverseCoordinateMatrix =
                        pgfLowLevel "xscale" (printNum $ 1/x) ++
                        pgfLowLevel "yscale" (printNum $ 1/y) ++
                        inverseCoordinateMatrix c 
                 }) $
  liftM (scope (numArgumentList [("xscale",x,""),("yscale",y,"")]))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Transform (Translate (x,y)) a) =
    local (\c -> c { coordinateMatrix = 
                      coordinateMatrix c ++ 
                      pgfLowLevel "xshift" (printNum x ++ "cm") ++
                      pgfLowLevel "yshift" (printNum y ++ "cm")
                 , inverseCoordinateMatrix =
                        pgfLowLevel "xshift" (printNum (-x) ++ "cm") ++
                        pgfLowLevel "yshift" (printNum (-y) ++ "cm") ++
                        inverseCoordinateMatrix c
                 }) $
  liftM (scope (numArgumentList [("xshift",x,"cm"),("yshift",y,"cm")]))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Composition a) =
   concat `liftM` mapM figureToTikzPictureWithStyle a

figureToTikzPictureWithStyle (Text a) = return $ node a

figureToTikzPictureWithStyle (Path a) = ask >>= \c ->
   let sp = getProperty (styleP c)
   in figurePath (pathToString a ++ 
                  (if sp closePath then " -- cycle" else ""))

figureToTikzPictureWithStyle (Circle ctr r) = 
  figurePath $ pointToString ctr ++ " circle (" ++ printNum r ++ "cm)"
     
figureToTikzPictureWithStyle (Grid v stepx stepy) = 
  figurePath $ "(0,0) grid[xstep=" 
  ++ printNum stepx ++ "cm,ystep=" 
  ++ printNum stepy ++ "]" 
  ++ pointToString v

-- * Drawing helpers

figurePath p = ask >>= \c ->
  let sp = getProperty (styleP c)
      fc = (scopePrefix (fillDepth c) ++ "fillc")
      lc = (scopePrefix (strokeDepth c) ++ "linec")
      at = (sp arrowTips)
  in return $ tikzCommand "path" (lineColorArgs sp fc lc at) p

-- * Style related commands

lineColorArgs sp fc lc at =
  arrowTipToArg at ++
  if sp clip then ["clip"] else
    if sp fill && sp stroke then ["fill=" ++ fc,"draw=" ++ lc]
    else if sp fill then ["fill="++fc] else ["draw="++lc]    
        
arrowTipToArg (TipNone,TipNone) = []
arrowTipToArg (l,r) = [leftTip l ++ "-" ++ rightTip r]

leftTip TipDefault = "<"
leftTip TipNone = ""
rightTip TipDefault = ">"
rightTip TipNone = ""

styleArguments :: StyleProperties -> [String]
styleArguments s =
  let sp = getProperty s
  in argumentList 
     ( extract lineCap (\p -> [("line cap", tikzLineCap p)]) ++ 
       extract lineJoin (\p -> [("line join", tikzLineJoin p)]) ++
       extract miterLimit (\p -> [("miter limit", printNum p)]) ++
       extract lineWidth (\p -> [("line width", printNum p)]) ++
       extract dashPhase (\p -> [("dash phase", printNum p)]) 
     )
     ++ maybe [] (\p ->
          if length p > 0 then
            argumentList [("dash pattern", dashPattern True p)]
          else
            ["solid"]) (dashes s)
  where extract prop f = maybe [] f (prop s)

tikzLineCap lc = case lc of
  CapRect -> "rect"
  CapButt -> "butt"
  CapRound -> "round"

tikzLineJoin lj = case lj of
  JoinRound -> "round"
  JoinBevel -> "bevel"
  JoinMiter -> "miter"

dashPattern :: Bool -> [Double] -> String
dashPattern b (x:xs) = (if b then "on " else "off ") ++ printNum x ++ " "
                       ++ dashPattern (not b) xs
dashPattern _ _ = ""

-- * TikZ/PGF & xcolor Commands

xcolor :: String -> FigureColor -> String
xcolor name color =
  let rgb = toSRGB color
  in texCommand "definecolor"
     [ name
     , "rgb"
     , printf "%.2f,%.2f,%.2f" 
       (channelRed rgb) 
       (channelGreen rgb) 
       (channelBlue rgb)]

prependColor name style prop =
  maybe (liftM id) (\p -> liftM (xcolor name p ++)) (prop style)

emptyScope body = environment "scope" [] body

scope [] body = body
scope args body = environment "scope" args body

node n = texCommand "node" [n]

pathToString = foldr ((++) . segmentToString) ""

segmentToString (MoveTo p) = 
  pointToString p
segmentToString (LineSegment p) = 
  " -- " ++ pointToString p
segmentToString (ArcSegment p sa ea r) = 
  -- This needs to be fixed, the point is only connected when there is a current point
  " -- " ++ pointToString p ++ " arc " ++ printf "(%f:%f:%fcm)" sa ea r
segmentToString (CurveSegment p c1 c2) =
  " .. controls " ++ pointToString c1 ++ "and " 
  ++ pointToString c2 ++ ".. " ++ pointToString p

pointToString (x,y) = "(" ++ printNum x ++ "," ++ printNum y ++ ") "

canvasTransform argList = 
  ["transform canvas=" ++ tikzSubArguments (numArgumentList argList)]

scopePrefix n = replicate n 'T'

pgfLowLevel t p = 
  texCommand "pgflowlevel" [texCommand' ("pgftransform" ++ t) [p]]

-- * TeX Output

printNum = printf "%f"

argumentList =
  map (\(l,n) -> l ++ "=" ++ n)

numArgumentList =
  map (\(l,n,u) -> l ++ "=" ++ printNum n ++ u)
  
tikzSubArguments :: [String] -> String
tikzSubArguments [] = ""
tikzSubArguments args = "{" ++ intercalate "," args ++ "}"

tikzArguments :: [String] -> String
tikzArguments [] = ""
tikzArguments args = "[" ++ intercalate "," args ++ "]"

texArguments :: [String] -> String
texArguments = concatMap (\s -> "{" ++ s ++ "}")

texCommand cmd args =
  "\\" ++ cmd ++ texArguments args ++ "\n"

texCommand' cmd args =
  "\\" ++ cmd ++ texArguments args

tikzCommand cmd args body =
  "\\" ++ cmd ++ tikzArguments args
  ++ " " ++ body ++ ";\n"

environment env args body =
  "\\begin{" ++ env ++ "}" ++ tikzArguments args ++ "\n"
  ++ body
  ++ "\\end{" ++ env ++ "}\n"

-- * Other helpers
maybeToInt = maybe 0 (const 1)

