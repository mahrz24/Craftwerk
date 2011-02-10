module Craftwerk.Core.Style where

import Craftwerk.Core.Color

import Data.List

-- This whole modules is a preliminary implementation only the interface itself won't change

type PList = [Property]

data Property = LineWidth Float
              | LineColor Color
              | FillColor Color
              | FillPattern Pattern
              | Fill Bool
              | Stroke Bool
              | ClosePath Bool
              deriving (Show, Eq)
                
data Pattern = Solid deriving (Show, Eq)


mergeProperties :: PList -> PList -> PList
mergeProperties _ a = a

fill :: PList -> Bool
fill s = (Fill True) `elem` s

stroke :: PList -> Bool
stroke s = (Stroke True) `elem` s

closePath :: PList -> Bool
closePath s = (ClosePath True) `elem` s

lineWidth :: PList -> Float
lineWidth s = extract $ find isLineWidth s
  where extract p = case p of 
          Just (LineWidth c) -> c
          _ -> 1.0
        isLineWidth p = case p of
          LineWidth _ -> True
          _ -> False


lineColor :: PList -> Maybe Color
lineColor s = extract $ find isLineColor s
  where extract p = case p of 
          Just (LineColor c) -> Just c
          _ -> Nothing
        isLineColor p = case p of
          LineColor _ -> True
          _ -> False

fillColor :: PList -> Maybe Color
fillColor s = extract $ find isFillColor s
  where extract p = case p of 
          Just (FillColor c) -> Just c
          _ -> Nothing
        isFillColor p = case p of
          FillColor _ -> True
          _ -> False