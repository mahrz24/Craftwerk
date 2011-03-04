-- |
-- Module      :  Craftwerk.Core.Driver.Tikz
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

data Context = Context { styleP :: StyleProperties 
                       , fillDepth :: Int
                       , strokeDepth :: Int
                       }

-- | Convert a Craftwerk 'Figure' to a TikZ picture environment string
figureToTikzPicture :: Figure -> String
figureToTikzPicture f =
  environment "tikzpicture" (styleArguments defaultStyle) $
  xcolor "linec" (getProperty defaultStyle lineColor) ++
  xcolor "fillc" (getProperty defaultStyle fillColor) ++
  (runReader (figureToTikzPictureWithStyle f) $
   Context { styleP = defaultStyle
           , fillDepth = 0
           , strokeDepth = 0})

figureToTikzPictureWithStyle :: Figure -> Reader Context String
figureToTikzPictureWithStyle Blank = return ""
figureToTikzPictureWithStyle (Style ns a) = 
  local (\c -> c { styleP = mergeProperties (styleP c) ns
                 , fillDepth = (fillDepth c) + (maybeToInt $ fillColor ns)
                 , strokeDepth = (strokeDepth c) + (maybeToInt $ lineColor ns)
                 }) $ (do
    c <- ask
    prependColor (scopePrefix (strokeDepth c) ++ "linec") ns lineColor
      $ prependColor (scopePrefix (fillDepth c) ++ "fillc") ns fillColor
      $ liftM (scope $ styleArguments ns)
      (figureToTikzPictureWithStyle a))


figureToTikzPictureWithStyle (Transform (Rotate r) a) =
  liftM (scope (numArgumentList [("rotate",r,"")]))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Transform (Scale (x,y)) a) =
  liftM (scope (numArgumentList [("xscale",x,"cm"),("yscale",y,"cm")]))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Transform (Translate (x,y)) a) =
  liftM (scope (numArgumentList [("xshift",x,"cm"),("yshift",y,"cm")]))
  (figureToTikzPictureWithStyle a)

figureToTikzPictureWithStyle (Composition a) =
   concat `liftM` mapM figureToTikzPictureWithStyle a

figureToTikzPictureWithStyle (Text a) = return $ node a

figureToTikzPictureWithStyle (Line a) = ask >>= \c ->
  let sp = getProperty (styleP c)
      fc = (scopePrefix (fillDepth c) ++ "fillc")
      lc = (scopePrefix (strokeDepth c) ++ "linec")
  in return $ tikzCommand "path" (lineColorArgs sp fc lc)
     ((pathToString a) ++ (if (sp closePath) then " -- cycle" else ""))
  where lineColorArgs sp fc lc =
          if (sp fill) && (sp stroke) then
            ["fill=" ++ fc,"draw=" ++ lc]
          else
            if (sp fill) then
              ["fill="++fc]
            else
              ["draw="++lc]

-- * Style related commands

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
          if (length p) > 0 then
            argumentList [("dash pattern", dashPattern True $ p)]
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

prependColor name style prop =
  maybe (liftM id) (\p -> liftM ((++) (xcolor name $ p))) (prop style)

scope args body = environment "scope" args body

node n = texCommand "node" [n]

pathToString p =
  intercalate " -- " $
  map (\(x,y) -> "(" ++ (printNum x) ++ "," ++ (printNum y) ++ ")") p

scopePrefix n = take n $ repeat 'T'

-- * TeX Output

printNum n = printf "%f" n

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

-- * Other helpers
maybeToInt s = maybe 0 (const 1) s

