-- |
-- Module      :  Craftwerk.Core.Figure
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- A 'Figure' is a description of a 2D vector graphic. Affine transformations
-- and styles apply to all subfigures. When a subfigure of a style node contains
-- another style node the style properties will be overwritten where the deeper
-- node specifies it.
--
module Craftwerk.Core.Figure (
  -- * Types and data types
    Point
  , Vector
  , Line
  , Path
  , Segment(..) 
  , Figure(..)
  , Transform(..)
    
    -- * Primitive wrappers
  , blank
  , canvas
  , rotate
  , scale
  , translate
  , composition
  , style
  , path
  , moveTo
  , lineTo
  , arc
  , curveTo
  , line
  , circle
  , grid
  , text

    -- * Point generation
  , point

    -- * Path generation
  , lineToPath
  , rectangle
  , unitRectangle
  ) where

import Craftwerk.Core.Style
import Craftwerk.Core.Color
import Data.Monoid

type Point = (Double, Double)
type Vector = Point

-- | Path creation
data Segment = MoveTo Point
             | LineSegment Point
             | ArcSegment Point Double Double Double
             | CurveSegment Point Point Point
             deriving (Show, Eq)
               
type Line = [Point]
type Path = [Segment]

-- | The main datatype describing an arbitrary figure.
data Figure = Blank
            | Transform Transform Figure
            | Canvas Transform Figure
            | Composition [Figure]
            | Style StyleProperties Figure
            | Path Path
            | Circle Point Double
            | Grid Vector Double Double
            | Text String
            | NoDecorations Figure
            deriving (Show, Eq)

data Transform = Rotate Double
               | Scale Vector
               | Translate Vector
               deriving (Show, Eq)

instance Monoid Figure where
  mempty = Blank
  mappend a b = Composition [a, b]
  mconcat = Composition
  
blank :: Figure
blank = Blank

canvas :: Figure -> Figure
canvas (Transform t f) = Canvas t f
canvas f = f

rotate :: Double -> Figure -> Figure
rotate r = Transform (Rotate r)
  
scale :: Vector -> Figure -> Figure
scale v = Transform (Scale v)

translate :: Vector -> Figure -> Figure
translate v = Transform (Translate v)

composition :: [Figure] -> Figure
composition = Composition

style :: StyleProperties -> Figure -> Figure
style = Style

line :: Line -> Figure
line l = Path (lineToPath l)

moveTo :: Point -> Segment
moveTo = MoveTo

lineTo :: Point -> Segment
lineTo = LineSegment

arc :: Point -> Double -> Double -> Double -> Segment
arc = ArcSegment

curveTo :: Point -> Point -> Point -> Segment
curveTo = CurveSegment

path :: Path -> Figure
path p = Path p

circle :: Point -> Double -> Figure
circle = Circle

grid :: Vector -> Vector -> Figure
grid v (x,y) = Grid v x y

text :: String -> Figure
text = Text

point :: Double -> Double -> Point
point x y = (x,y)

lineToPath :: Line -> Path
lineToPath (p:ps) = (MoveTo p):(map (\p -> LineSegment p) ps)

-- | Construct a rectangle path from origin and extent.
rectangle :: Point -> Vector -> Line
rectangle (x,y) (w,h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

-- | Rectangle with origin (0,0) and extent (1,1)
unitRectangle :: Line
unitRectangle = rectangle (0,0) (1,1)
