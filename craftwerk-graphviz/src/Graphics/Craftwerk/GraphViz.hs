-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Craftwerk.Graphviz
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
-- Stability   :  unstable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Craftwerk.GraphViz (

) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Example

import Data.GraphViz hiding (red)
import Data.Either
import Data.List

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI

import Debug.Trace

import Data.Complex

import Text.ParserCombinators.Parsec


main :: IO ()
main = return ()

m486 :: NodeMap String
m486 = fromGraph clr486

t1 :: Gr String ()
t1 = insMapEdge m486 ("shirt", "watch", ()) clr486

t2 :: Gr String ()
t2 = insMapEdge m486 ("watch", "pants", ()) t1

t3 :: Gr Char String
t3 = run_ empty $
    do insMapNodeM 'a'
       insMapNodeM 'b'
       insMapNodeM 'c'
       insMapEdgesM [('a', 'b', "right"),
		     ('b', 'a', "left"),
		     ('b', 'c', "down"),
		     ('c', 'a', "up")]

t4 :: Gr String ()
t4 = run_ clr486 $ insMapEdgeM ("shirt", "watch", ())

defaultVis :: (Graph gr) => gr nl el -> DotGraph Node
defaultVis = graphToDot nonClusteredParams

evenOdd :: (Graph gr, Ord el) => gr String el -> DotGraph Node
evenOdd = setDirectedness graphToDot params
  where
    params = defaultParams { globalAttributes = []
                --         , clusterBy        = clustBy
                         , clusterID        = Just . Int
                         , fmtCluster       = clFmt
                         , fmtNode          = ndFmt
                         , fmtEdge          = const []
                         }
    clustBy (n,l) = C (n `mod` 2) $ N (n,l)
    clFmt m = [GraphAttrs [toLabel $ "n == " ++ show m ++ " (mod 2)"]]
    ndFmt (n,l) = [toLabel l]

xdotize g = graphvizWithHandle Dot g XDot hGetContents'

tada :: (Graph gr, Ord el) => gr String el -> IO (gr Attributes Attributes)
tada g = do x <- xdotize $ evenOdd g
            let parsed = (parseDotGraph (either (const "") id $ x)) :: DotGraph Node
            return $ dotToGraph parsed


in1 = sliderIIn (0,3) 0

graphs = [clr486, t1, t2, t4]

ccc :: (Graph gr) => [gr Attributes Attributes] -> Int ->  Figure
ccc graphs i = let g = (graphs !! i)
               in style newStyle { fill = yes, closePath = yes, fillColor = Just red} $ scale (0.002 :+ 0.002) $ composition [mapGraphToFigure g]

qm = do g <- mapM tada graphs
        runGTVInWindow "Bla" $ pinski (ccc g)


pinski :: (Int -> Figure) -> GTV (Int -> Figure)
pinski f = tv
           (oTitle "Graphouput" $ oLambda (iTitle "Graph selection" in1) (figureOut 1.0 1.0)) f

--- Sample code & xdot conversion above


-- Convert an annotized graph to a figure

mapGraphToFigure :: (Graph gr) => gr Attributes Attributes ->  Figure
mapGraphToFigure g =
  let n = labNodes g
      e = labEdges g
  in composition [drawNodes n, drawEdges e]
  where drawNodes n = composition $ map nodeToFigure n
        drawEdges e = composition $ map edgeToFigure e

filterAttributes :: Attributes -> [String]
filterAttributes = map xDotArg . (filter isXDot)
  where isXDot (UnknownAttribute nme arg) = elem nme ["_draw_"
                                                     ,"_ldraw_"
                                                     ,"_hdraw_"
                                                     ,"_tdraw_"
                                                     ,"_hldraw_"
                                                     ,"_tldraw_"]
        isXDot _ = False
        xDotArg (UnknownAttribute nme arg) = arg
        xDotArg _ = ""

nodeToFigure :: LNode Attributes -> Figure
nodeToFigure (n,a) = composition $ map xDotToFigure (filterAttributes a)

edgeToFigure :: LEdge Attributes -> Figure
edgeToFigure (n1,n2,a) = composition $ map xDotToFigure (filterAttributes a)

xDotToFigure :: String -> Figure
xDotToFigure s = let p = either (const []) id (parse primitivesParser "" s)
                 in composition $ primitivesToFigure p

data XDotPrim  = XUnfilledEllipse (Complex Double) Double Double
                | XFilledEllipse (Complex Double) Double Double
                | XUnfilledPolygon [Complex Double]
                | XFilledPolygon [Complex Double]
                | XPolyLine [Complex Double]
                | XUnfilledBSpline [Complex Double]
                | XFilledBSpline [Complex Double]
                | XText (Complex Double) String
                | XStrokeColor (Colour Double)
                | XFillColor (Colour Double)
                | XStyle String

primitivesParser :: Parser [XDotPrim]
primitivesParser = sepEndBy1 primitive (many1 space)

primitive :: Parser XDotPrim
primitive = unfilledEllipse <|> filledEllipse

unfilledEllipse :: Parser XDotPrim
unfilledEllipse = do char 'e'
                     space
                     a <- numberAsDouble
                     return $ XUnfilledEllipse origin 0 0


filledEllipse = undefined

numberAsDouble :: Parser Double
numberAsDouble = do n <- many1 digit
                    return $ fromInteger (read n)

--parsePrimitives (prims,x:xs) =
--  case trace xs $ x of
--    'e' -> parsePrimitives $ parseEllipse prims xs
--    'c' -> parsePrimitives $ parseStrokeColor prims xs
--    'C' -> parsePrimitives $ parseStrokeColor prims xs
--    ' ' -> parsePrimitives (prims,xs)
--    'F' -> parsePrimitives $ parseFont prims xs
--    'T' -> parsePrimitives $ parseText prims xs
--    'B' -> parsePrimitives $ parseBSpline prims xs
--    -- 'P' -> parsePrimitives $ parsePolygon prims xs
--    _ -> []
--parsePrimitives (prims,_) = prims
--
--parseStrokeColor prims xs =
--  let ws = words $ tail xs
--      n = read $ ws !! 0
--  in (prims, drop (n+1) $ intercalate " " $ tail ws)
--
--parseFont prims xs =
--  let ws = words $ tail xs
--      n = read $ ws !! 1
--  in (prims, drop (n+1) $ intercalate " " $ drop 2 ws)
--
--parseBSpline prims xs =
--  let ws = words $ tail xs
--      n = read $ ws !! 1
--  in (prims, drop (n+1) $ intercalate " " $ drop 2 ws)
--
--parseText prims xs =
--  let ws = words $ tail xs
--      x = read $ ws !! 0
--      y = read $ ws !! 1
--      w = read $ ws !! 3
--      n = read $ ws !! 4
--      nxs = intercalate " " $ drop 5 ws
--  in ((XText (x-w/3) y (take (n+1) $ tail nxs):prims), drop (n+1) nxs )
--
--parseEllipse prims xs =
--  let ws = words $ tail xs
--      x = read $ ws !! 0
--      y = read $ ws !! 1
--      w = read $ ws !! 2
--      h = read $ ws !! 3
--  in ((XUnfilledEllipse x y w h):prims,intercalate " " $ drop 4 ws)

primitivesToFigure :: [XDotPrim] -> [Figure]
primitivesToFigure ((XUnfilledEllipse p w h):ps) =
  (style newStyle {fill = no} $ translate p $ scale (w :+ h) $ circle origin 1.0):(primitivesToFigure ps)
primitivesToFigure ((XText p s):ps) =
  (style newStyle {fill = yes, fillColor = Just black, stroke = no} $ translate p $ text s):(primitivesToFigure ps)
primitivesToFigure _ = []


