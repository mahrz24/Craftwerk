module Craftwerk.Core.Style where

import Craftwerk.Core.Color
import Craftwerk.Core.ColorNames

import Data.Maybe


data StyleProperties = StyleProperties { lineWidth :: Maybe Float
                                       , lineColor :: Maybe Color
                                       , fillColor :: Maybe Color
                                       , fill :: Maybe Bool
                                       , stroke :: Maybe Bool 
                                       , closePath :: Maybe Bool
                                       } deriving (Show, Eq) 


emptyStyle = StyleProperties Nothing Nothing Nothing Nothing Nothing Nothing

defaultStyle = StyleProperties { lineWidth = Just 1.0
                               , lineColor = Just black
                               , fillColor = Just white
                               , stroke = Just True
                               , fill = Just False
                               , closePath = Just False 
                               }               
               
yes = Just True
no = Just False



getProperty :: StyleProperties -> (StyleProperties -> Maybe a) -> a
getProperty s f = fromMaybe (fromJust $ f defaultStyle) (f s)

mergeProperty :: StyleProperties -> StyleProperties -> (StyleProperties -> Maybe a) -> Maybe a
mergeProperty s t f = case (f t) of
  Nothing -> (f s)
  x -> x

mergeProperties :: StyleProperties -> StyleProperties -> StyleProperties
mergeProperties s t = StyleProperties { lineWidth = mergeProperty s t lineWidth
                                      , lineColor = mergeProperty s t lineColor
                                      , fillColor = mergeProperty s t fillColor
                                      , fill = mergeProperty s t fill
                                      , stroke = mergeProperty s t stroke
                                      , closePath = mergeProperty s t closePath
                                      }