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

  -- * Path generation
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

-- | Construct a rectangle path from origin and extent.
rectangle :: Point -> Vector -> Path
rectangle (x,y) (w,h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

-- | Rectangle with origin (0,0) and extent (1,1)
unitRectangle :: Path
unitRectangle = rectangle (0,0) (1,1)
