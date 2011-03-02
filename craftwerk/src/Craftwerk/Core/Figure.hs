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
  , Path
  , Figure(..)
  , Transform(..)
    
    -- * Primitive wrappers
  , blank
  , rotate
  , scale
  , translate
  , composition
  , style
  , line
  , text

    -- * Point generation
  , point

    -- * Path generation
  , path
  , rectangle
  , unitRectangle
  ) where

import Craftwerk.Core.Style
import Craftwerk.Core.Color
import Data.Monoid

type Point = (Float, Float)
type Vector = Point
type Path = [Point]

-- | The main datatype describing an arbitrary figure.
data Figure = Blank
            | Transform Transform Figure
            | Composition [Figure]
            | Style StyleProperties Figure
            | Line Path
            | Text String
            deriving (Show, Eq)

data Transform = Rotate Float
               | Scale Vector
               | Translate Vector
               deriving (Show, Eq)

instance Monoid Figure where
  mempty = Blank
  mappend a b = Composition [a, b]
  mconcat = Composition
  
blank :: Figure
blank = Blank

rotate :: Float -> Figure -> Figure
rotate r = Transform (Rotate r)
  
scale :: Vector -> Figure -> Figure
scale v = Transform (Scale v)

translate :: Vector -> Figure -> Figure
translate v = Transform (Translate v)

composition :: [Figure] -> Figure
composition = Composition

style :: StyleProperties -> Figure -> Figure
style = Style

line :: Path -> Figure
line = Line

text :: String -> Figure
text = Text

point :: Float -> Float -> Point
point x y = (x,y)

path :: [Point] -> Path
path = id

-- | Construct a rectangle path from origin and extent.
rectangle :: Point -> Vector -> Path
rectangle (x,y) (w,h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

-- | Rectangle with origin (0,0) and extent (1,1)
unitRectangle :: Path
unitRectangle = rectangle (0,0) (1,1)
