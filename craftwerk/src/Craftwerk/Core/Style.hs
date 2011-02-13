-- |
-- Module      :  Craftwerk.Core.Style
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
--

module Craftwerk.Core.Style (
  -- * Data types
    StyleProperties(
                     StyleProperties
                   , lineWidth
                   , lineColor
                   , fillColor
                   , stroke
                   , fill
                   , closePath
                   )

  -- * Named styles
  , emptyStyle
  , defaultStyle

  -- * Property values
  , yes
  , no

  -- * Property access and merging
  , getProperty
  , mergeProperties

  ) where

import Craftwerk.Core.Color
import Craftwerk.Core.ColorNames

import Data.Maybe

-- | A record holding all possible properties.
data StyleProperties =
  StyleProperties { lineWidth :: Maybe Float
                  , lineColor :: Maybe Color
                  , fillColor :: Maybe Color
                  , fill :: Maybe Bool
                  , stroke :: Maybe Bool
                  , closePath :: Maybe Bool
                  } deriving (Show, Eq)

-- | A style where no property has been set.
emptyStyle :: StyleProperties
emptyStyle = StyleProperties Nothing Nothing Nothing Nothing Nothing Nothing

-- | The default style used at the root node of any 'Figure'.
defaultStyle :: StyleProperties
defaultStyle =
  StyleProperties { lineWidth = Just 1.0
                  , lineColor = Just black
                  , fillColor = Just white
                  , stroke = Just True
                  , fill = Just False
                  , closePath = Just False
                  }

-- | Alias for 'Just True' to make style specification more convenient.
yes :: Maybe Bool
yes = Just True

-- | Alias for 'Just False' to make style specification more convenient.
no :: Maybe Bool
no = Just False

-- | Read a property from a style property record returning the value of
-- the default style if the value is 'Nothing'.
getProperty :: StyleProperties -> (StyleProperties -> Maybe a) -> a
getProperty s f = fromMaybe (fromJust $ f defaultStyle) (f s)

mergeProperty :: StyleProperties ->
                 StyleProperties ->
                 (StyleProperties -> Maybe a) ->
                 Maybe a
mergeProperty s t f = case (f t) of
  Nothing -> (f s)
  x -> x

-- | Merge two property records, where the second argument overwrites fields of
-- the first unless a field is 'Nothing'.
mergeProperties :: StyleProperties ->
                   StyleProperties ->
                   StyleProperties
mergeProperties s t = StyleProperties { lineWidth = mergeProperty s t lineWidth
                                      , lineColor = mergeProperty s t lineColor
                                      , fillColor = mergeProperty s t fillColor
                                      , fill = mergeProperty s t fill
                                      , stroke = mergeProperty s t stroke
                                      , closePath = mergeProperty s t closePath
                                      }
