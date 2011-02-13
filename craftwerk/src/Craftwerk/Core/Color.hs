-- |
-- Module      :  Craftwerk.Core.Color
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.Core.Color (
  -- * Data type
    Color(RGBA)

  -- * Color construction
  , makeColor
  , makeColorFromIntegral

  -- * Component access
  , getRGB
  , getRGBA

  -- * Color blending
  , (!)

  -- * Map
  , cmap
    
  ,cap

  ) where

-- | Color levels are in the range [0 .. 1]
data Color = RGBA Float Float Float Float deriving (Show, Eq)

-- | Create a color from red, green and blue value. Color values are capped.
makeColor :: Float -> Float -> Float -> Color
makeColor r g b = RGBA (cap r) (cap g) (cap b) 1.0

-- | Create a color from red, green and blue value in range [0 .. 100].
-- Color values are capped.
makeColorFromIntegral :: (Integral a) => a -> a -> a -> Color
makeColorFromIntegral r g b = RGBA (convert r) (convert g) (convert b) 1.0
  where convert k = cap $ (fromIntegral k) / 100.0

-- | Create a color from red, green, blue and alpha value.
-- Color values are capped.
makeColorWithAlpha :: Float -> Float -> Float -> Float -> Color
makeColorWithAlpha r g b a = RGBA (cap r) (cap g) (cap b) (cap a)

-- | Extract RGB values.
getRGB :: Color -> (Float, Float, Float)
getRGB (RGBA r g b a) = (r,g,b)

-- | Extract RGBA values.
getRGBA :: Color -> (Float, Float, Float, Float)
getRGBA (RGBA r g b a) = (r,g,b,a)

-- | Blend colors with integer percentage value (TODO: this is not as TikZ
-- or xcolor do it!!)
(!) :: Color -> Int -> Color
(!) (RGBA r g b a) i = RGBA r g b (a*(fromIntegral i)/100.0)


-- | Transform colors with a component wise map. Results get capped
cmap :: (Float -> Float) -> Color -> Color
cmap f (RGBA r g b a) = RGBA (cap $ f r) (cap $ f g) (cap $ f b) a

cap :: Float -> Float
cap = min 1 . max 0

