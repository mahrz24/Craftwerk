-- |
-- Module      :  Graphics.Craftwerk.Core.Figure
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- A 'Figure' is a description of a 2D vector graphic. Affine transformations
-- and styles apply to all subfigures. When a subfigure of a style node contains
-- another style node the style properties will be overwritten where the deeper
-- node specifies it.
--
module Graphics.Craftwerk.Core.Figure (
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

import Graphics.Craftwerk.Core.Style
import Graphics.Craftwerk.Core.Color
import Data.Monoid

type Point = (Double,Double)
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
            | Decoration Point Figure
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

-- | Transforms a coordinate transformation 'Transform t f' into
-- a canvas transformation  'Canvas t f'.
canvas :: Figure -> Figure
canvas (Transform t f) = Canvas t f
canvas f = f

-- | Applies a coordinate rotation.
rotate :: Double -> Figure -> Figure
rotate r = Transform (Rotate r)

-- | Applies a coordinate scaling.
scale :: Vector -> Figure -> Figure
scale v = Transform (Scale v)

-- | Applies a coordinate translation.
translate :: Vector -> Figure -> Figure
translate v = Transform (Translate v)

-- | Combines figures which are drawn in the order of the figures in the list.
composition :: [Figure] -> Figure
composition = Composition

-- | Applies a style to the given figure
style :: StyleProperties -> Figure -> Figure
style = Style

-- | Converts a line to a figure
line :: Line -> Figure
line l = Path (lineToPath l)

-- | Generates a 'MoveTo' segment
moveTo :: Point -> Segment
moveTo = MoveTo

-- | Generates a 'LineSegment' segment.
lineTo :: Point -> Segment
lineTo = LineSegment

-- | Generates an 'ArcSegment' segment.
arc :: Point -- ^ Draws a line to this point unless segment is the first element in the  path.
       -> Double -- ^ Start angle
       -> Double -- ^ End angle
       -> Double -- ^ Radius
       -> Segment
arc = ArcSegment

-- | Generates a 'CurveSegment' segment.
curveTo :: Point -- ^ End point of the segment.
           -> Point -- ^ Control point 1.
           -> Point -- ^ Control point 2.
           -> Segment
curveTo = CurveSegment

-- | Generate a figure from a path.
path :: Path -> Figure
path = Path

-- | Generate a circle figure at a point with given radius.
circle :: Point -> Double -> Figure
circle = Circle

-- | Generate a grid spanning from (0,0) to (x,y) taking steps (xs,ys).
grid :: Vector -- ^ x,y
        -> Vector -- ^ xs,ys
        -> Figure
grid v (x,y) = Grid v x y

-- | Generate a text at (0,0)
text :: String -> Figure
text = Text

point :: Double -> Double -> Point
point x y = (x,y)

lineToPath :: Line -> Path
lineToPath (p:ps) = MoveTo p:map LineSegment ps

-- | Construct a rectangle path from origin and extent.
rectangle :: Point -- ^ Origin
             -> Vector -- ^ Extent
             -> Line
rectangle (x,y) (w,h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

-- | Rectangle with origin (0,0) and extent (1,1)
unitRectangle :: Line
unitRectangle = rectangle (0,0) (1,1)
