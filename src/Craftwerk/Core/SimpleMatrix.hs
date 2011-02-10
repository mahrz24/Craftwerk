module Craftwerk.Core.SimpleMatrix where

import Data.List

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

instance Num a => Num (Matrix a) where
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    Matrix as * Matrix bs =
       Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
    negate (Matrix as) = Matrix (map (map negate) as)
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1

apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]

apply2D :: Num a => Matrix a -> (a,a) -> (a,a)
apply2D m (x,y) = let r = apply m [x,y]
                  in (r !! 0, r !! 1)