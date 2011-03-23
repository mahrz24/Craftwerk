-- |
-- Module      :  Graphics.Craftwerk.Core.Driver.Generic
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- Functions that convert figures into figures that use less
-- primitives such that drivers only need to implement basic
-- functionalities.

module Graphics.Craftwerk.Core.Driver.Generic (
  -- * Internal conversion to simpler primitives
  genericLevel2Figure
  , arrowTipsForPath
    -- * Conversion helpers
  , radians
  , degree
  ) where

import Graphics.Craftwerk.Core.Figure
import Graphics.Craftwerk.Core.Color
import Graphics.Craftwerk.Core.Style

import Data.VectorSpace

-- | Converts figures using Level 3 (TikZ) features to Level 2 (Cairo) features.
genericLevel2Figure :: Figure -> Figure
genericLevel2Figure (Circle (x,y) r) =
  Path [ArcSegment (x+r,y) 0 360  r]

genericLevel2Figure (Grid (x,y) xs ys) =
  composition $
  [line [(0,fromIntegral i * ys),(x,fromIntegral i * ys)] | i <- [0 .. floor (y/ys)]] ++
  [line [(fromIntegral i *xs,0),(fromIntegral i * xs,y)] | i <- [0 .. floor (x/xs)]]

-- Everything else is a valid level 2 figure
genericLevel2Figure f = f

-- Methods to express level 3 styles as level 2 figures

-- | Creates arrow tip figure for a given path
arrowTipsForPath :: [Segment] -> Double -> (ArrowTip, ArrowTip) -> Figure
arrowTipsForPath p lw at =
  composition $ foldr (tipForSegment lw at) []
  (adjacent $ MoveTo (0,0):p ++ [MoveTo (0,0)])

tipForSegment lw (l,r) (s1,s2,s3) ats =
  ats ++ leftTipForSegment lw l s1 s2 ++ rightTipForSegment lw r s1 s2 s3

leftTipForSegment _ _ (MoveTo _) (MoveTo _) = []
leftTipForSegment lw l s1@(MoveTo p) s2 = arrowTipForTangent lw l (leftTangent s1 s2)
leftTipForSegment _ _ _ _ = []

rightTipForSegment _ _ _ (MoveTo _) (MoveTo _) = []
rightTipForSegment lw r s1 s2 (MoveTo p) = arrowTipForTangent lw r (rightTangent s1 s2)
rightTipForSegment _ _ _ _ _ = []

data Tangent = NoTangent | Tangent Point Point deriving (Eq, Show)

tangent p1 p2 = if p1 == p2 then
                  NoTangent
                else Tangent p1 (normalized (p1 ^-^ p2))

leftTangent (MoveTo p1) (LineSegment p2) = tangent p1 p2
leftTangent (MoveTo p1) (CurveSegment _ p2 _) = tangent p1 p2
leftTangent (MoveTo p1) (ArcSegment p2 _ _ _) = tangent p1 p2
leftTangent _ _ = NoTangent

rightTangent (MoveTo p1) (LineSegment p2) = tangent p2 p1
rightTangent (LineSegment p1) (LineSegment p2) = tangent p2 p1
rightTangent (CurveSegment p1 _ _) (LineSegment p2) = tangent p2 p1
rightTangent _ (CurveSegment p1 _ p2) = tangent p1 p2
rightTangent _ _ = NoTangent

arrowTipForTangent _ _ NoTangent = []
arrowTipForTangent _ TipNone _ = []
arrowTipForTangent lw TipDefault t =
  let arrowsize = 0.8 + 2*lw
  in arrowTip t $ style newStyle { lineCap = Just CapRound } $ composition
     [ path [ArcSegment (0,0) 90 180 arrowsize]
     , path [ArcSegment (0,0) 270 180 arrowsize]]

arrowTip (Tangent p (x,y)) f =
  [translate p $ Decoration p $ rotate (degree $ -(atan2 y x)) $ f]

adjacent :: [a] -> [(a,a,a)]
adjacent xs = zipWith3 triple xs (tail xs) (tail $ tail xs)
  where triple a b c = (a,b,c)

-- General methods used in drivers

-- | Angle conversion
radians :: (Floating a) => a -> a
radians n = n / (360 / (2 * pi))

-- | Angle conversion
degree :: (Floating a) => a -> a
degree n = n / ((2*pi) / 360)

