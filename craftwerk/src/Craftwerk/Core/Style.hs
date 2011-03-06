-- |
-- Module      :  Craftwerk.Core.Style
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.Core.Style (
  -- * Data types
    StyleProperties(..)
    
    , LineCap(
               CapRect
             , CapButt
             , CapRound
             )
    
    , LineJoin(
                JoinRound
              , JoinBevel
              , JoinMiter
              )

  -- * Named styles
  , emptyStyle
  , newStyle
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

data LineCap = CapRect | CapButt | CapRound deriving (Show,Eq)
data LineJoin = JoinRound | JoinBevel | JoinMiter deriving (Show,Eq)

-- | A record holding all possible properties.
data StyleProperties =
  StyleProperties { lineWidth :: Maybe Float
                  , lineColor :: Maybe Color
                  , fillColor :: Maybe Color
                  , fill :: Maybe Bool
                  , stroke :: Maybe Bool
                  , clip :: Maybe Bool
                  , closePath :: Maybe Bool
                  , dashes :: Maybe [Float]
                  , dashPhase :: Maybe Float
                  , lineCap :: Maybe LineCap
                  , lineJoin :: Maybe LineJoin
                  , miterLimit :: Maybe Float
                  } deriving (Show, Eq)

-- | A style where no property has been set.
emptyStyle :: StyleProperties
emptyStyle = StyleProperties 
             Nothing 
             Nothing 
             Nothing 
             Nothing
             Nothing 
             Nothing 
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             
-- | Alias for empty style, makes style construction look nicer
newStyle = emptyStyle

-- | The default style used at the root node of any 'Figure'.
defaultStyle :: StyleProperties
defaultStyle =
  StyleProperties { lineWidth = Just 1.0
                  , lineColor = Just black
                  , fillColor = Just white
                  , stroke = Just True
                  , fill = Just False
                  , clip = Just False
                  , closePath = Just False
                  , dashes = Just [] :: Maybe [Float]
                  , dashPhase = Just 0.0
                  , lineCap = Just CapButt
                  , lineJoin = Just JoinMiter
                  , miterLimit = Just 10.0
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
mergeProperties s t = 
  StyleProperties { lineWidth = mergeProperty s t lineWidth
                  , lineColor = mergeProperty s t lineColor
                  , fillColor = mergeProperty s t fillColor
                  , fill = mergeProperty s t fill
                  , stroke = mergeProperty s t stroke
                  , clip = mergeProperty s t clip
                  , closePath = mergeProperty s t closePath
                  , dashes = mergeProperty s t dashes
                  , dashPhase = mergeProperty s t dashPhase
                  , lineCap = mergeProperty s t lineCap
                  , lineJoin = mergeProperty s t lineJoin
                  , miterLimit = mergeProperty s t miterLimit
                  }

