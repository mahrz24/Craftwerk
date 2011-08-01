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
import qualified Data.GraphViz.Parsing as GP
import Data.Either
import Data.List

import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI

import Debug.Trace

import Data.Complex

import Data.Char

import Data.Colour
import Data.HashTable

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



td :: String -> IO (Gr Attributes Attributes)
td fn = do fc <- readFile fn
           let g = (parseDotGraph fc) :: DotGraph String
           gv <- xdotize (trace (show g) g)
           let xd = (either (const "") id $ gv)
           let reparsed = (parseDotGraph (trace xd xd)) :: DotGraph String
           let ir = fmap (fromIntegral . hashString) reparsed
           return $ dotToGraph ir

--xdotload filename =

in1 = sliderIIn (0,2) 0

graphs = [clr486, t1, t2, t4]
ngraphs = ["fsm.gv.txt","crazy.gv.txt", "world.gv.txt"]

ccc :: (Graph gr) => [gr Attributes Attributes] -> Int ->  Figure
ccc graphs i = let g = (graphs !! i)
               in style newStyle { fill = yes, closePath = yes, fillColor = Just red}
               $ composition [mapGraphToFigure g]

qm = do g <- mapM td ngraphs
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
xDotToFigure s = let p = either (const []) id $ (parse primitivesParser "" s)
                 in composition $ primitivesToFigure p

data XFill = XFill | XStroke deriving (Show, Eq)
data XAlign = XLeft | XCenter | XRight deriving (Show, Eq)

data XDotPrim  = XEllipse XFill (Complex Double) Double Double
                | XPolygon XFill [Complex Double]
                | XPolyLine [Complex Double]
                | XBSpline XFill [Complex Double]
                | XText (Complex Double) XAlign Double String
                | XStrokeColor (Colour Double)
                | XFillColor (Colour Double)
                | XFont Double String
                | XStyle [(String, [String])]
                | XImage
                deriving (Show, Eq)

primitivesParser :: Parser [XDotPrim]
primitivesParser = sepEndBy1 primitive (many space)

primitive :: Parser XDotPrim
primitive =  ellipse
         <|> polygon
         <|> polyline
         <|> bspline
         <|> txt
         <|> strokeColor
         <|> fillColorP
         <|> font
         <|> styleP
         <|> imageP

ellipse :: Parser XDotPrim
ellipse = do t <- pathType 'e'
             space
             p <- numbersAsComplex
             space
             w <- numberAsDouble
             space
             h <- numberAsDouble
             return $ XEllipse t p w h

polygon :: Parser XDotPrim
polygon = do t <- pathType 'p'
             space
             pts <- points
             return $ XPolygon t pts

polyline :: Parser XDotPrim
polyline = do char 'L'
              space
              pts <- points
              return $ XPolyLine pts

bspline :: Parser XDotPrim
bspline = do t <- pathType 'b'
             space
             pts <- points
             return $ XBSpline t pts

txt :: Parser XDotPrim
txt = do char 'T'
         space
         p <- numbersAsComplex
         space
         a <- alignment
         space
         w <- numberAsDouble
         space
         t <- fwString
         return $ XText p a w t

strokeColor :: Parser XDotPrim
strokeColor = do char 'c'
                 space
                 c <- strColor
                 return $ XStrokeColor c

fillColorP :: Parser XDotPrim
fillColorP = do char 'C'
                space
                c <- strColor
                return $ XFillColor c

font :: Parser XDotPrim
font = do char 'F'
          space
          s <- sizeP
          space
          fn <- fwString
          return $ XFont s fn

styleP :: Parser XDotPrim
styleP  = do char 'S'
             space
             s <- fwString
             return $ XStyle (either (const []) id (parse styleParser "" s))

imageP :: Parser XDotPrim
imageP = do char 'I'
            space
            numbersAsComplex
            space
            numberAsDouble
            space
            numberAsDouble
            space
            fwString
            return $ XImage

styleParser :: Parser [(String, [String])]
styleParser = sepBy1 styleItemP (char ',')

styleItemP :: Parser (String, [String])
styleItemP = do n <- many1 alphaNum
                args <- option [] (between (char '(') (char ')') $ sepBy1 (many1 alphaNum) (char ','))
                return (n,args)

-- Helper parsers

sizeP :: Parser Double
sizeP = do i <- many digit
           d <- try (char '.' >> try (many (digit)))
           return $ (read (i++"."++d))

strColor :: Parser (Colour Double)
strColor = do s <- fwString
               -- Now parse that string with graphviz again
              let col = GP.runParser' GP.parseUnqt s :: Color
              return (maybe black (flip over black) (toColour col))

fwString :: Parser String
fwString = do n <- numberAsInt
              space
              char '-'
              count n anyChar

alignment :: Parser XAlign
alignment = do a <- ((do s <- char '-'
                         d <- digit
                         return [s,d]) <|> ((\x -> return [x]) =<< digit))
               return $ case a of
                   "-1" -> XLeft
                   "0" -> XCenter
                   _ -> XRight

points :: Parser [Complex Double]
points = do n <- numberAsInt
            space
            count n (do pt <- numbersAsComplex
                        many space
                        return pt)

numberAsDouble :: Parser Double
numberAsDouble = do n <- many1 digit
                    return $ (fromInteger (read n))/1000

numberAsInt :: Parser Int
numberAsInt = do n <- many1 digit
                 return (read n)

numbersAsComplex :: Parser (Complex Double)
numbersAsComplex = do re <- numberAsDouble
                      space
                      im <- numberAsDouble
                      return (re :+ im)

pathType t = do f <- oneOf [t,(toUpper t)]
                return $ fillCase f

fillCase f = case (isUpper f) of
              True -> XFill
              _ -> XStroke

every :: Int -> Int -> [a] -> [[a]]
every m n p@(x:xs) = (take m p):(every m n (drop n p))
every m n [] = []

ozip x = zip4 x (drop 1 x) (drop 2 x) (drop 3 x)

seg s = case (length s) of
           4 -> [moveTo (s !! 0), curveTo (s !! 3) (s !! 1) (s !! 2)]
           _ -> []

splineSegs = concatMap seg . every 4 3

primitivesToFigure :: [XDotPrim] -> [Figure]
primitivesToFigure ((XEllipse f p w h):ps) =
  (style newStyle {fill = no} $ translate p $ scale (w :+ h) $ circle origin 1.0):(primitivesToFigure ps)
primitivesToFigure ((XPolygon f pts):ps) =
  (style newStyle {fill = no} $ path $ lineToPath pts):(primitivesToFigure ps)
primitivesToFigure ((XBSpline f pts):ps) =
  (style newStyle {fill = no, closePath = no} $ path $ splineSegs pts):(primitivesToFigure ps)
primitivesToFigure ((XText p a w s):ps) =
  (style newStyle {fill = yes, fillColor = Just black, stroke = no} $ translate p $ scale (0.001 :+ 0.001) $ text s):(primitivesToFigure ps)
primitivesToFigure (_:ps) = primitivesToFigure ps
primitivesToFigure _ = []


