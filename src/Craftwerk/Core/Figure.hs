module Craftwerk.Core.Figure where

import Craftwerk.Core.Style
import Craftwerk.Core.Color
import Data.Monoid

import qualified Data.Vector as Vec

type Point = (Float, Float)
type Vector = Point

type Path = [Point]

data Figure = Blank 
            | Rotate Float Figure
            | Scale Vector Figure
            | Translate Vector Figure
            | Composition [Figure]
            | Style StyleProperties Figure
            | Line Path
            | Text String
            deriving (Show, Eq)

data Picture = Picture Int Int (Vec.Vector Color) deriving (Show, Eq)

instance Monoid Figure where
  mempty = Blank
  mappend a b = Composition [a, b]
  mconcat = Composition
  
rectangle :: Point -> Vector -> Path
rectangle (x,y) (w,h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

unitRectangle = rectangle (0,0) (1,1)