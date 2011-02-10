module Craftwerk.Core.Color where

data Color = RGBA Float Float Float Float deriving (Show, Eq)

makeColor :: Float -> Float -> Float -> Color
makeColor r g b = RGBA (cap r) (cap g) (cap b) 1.0

makeColorFromTriple :: (Float, Float, Float) -> Color
makeColorFromTriple (r,g,b) = makeColor r g b

makeColorFromIntegral :: (Integral a) => a -> a -> a -> Color
makeColorFromIntegral r g b = RGBA (convert r) (convert g) (convert b) 1.0
  where convert k = cap $ (fromIntegral k) / 100.0

makeColorWithAlpha :: Float -> Float -> Float -> Float -> Color
makeColorWithAlpha r g b a = RGBA (cap r) (cap g) (cap b) (cap a)

getRGB :: Color -> (Float, Float, Float)
getRGB (RGBA r g b a) = (r,g,b)

getRGBA :: Color -> (Float, Float, Float, Float)
getRGBA (RGBA r g b a) = (r,g,b,a)

cap :: Float -> Float
cap = min 1 . max 0

cmap :: (Float -> Float) -> Color -> Color
cmap f (RGBA r g b a) = RGBA (cap $ f r) (cap $ f g) (cap $ f b) a

(!) :: Color -> Int -> Color
(!) (RGBA r g b a) i = RGBA r g b (a*(fromIntegral i)/100.0) 

rampColorHotToCold :: Float -> Float -> Float -> Color
rampColorHotToCold vmin vmax vNotNorm
 = let	
	v	| vNotNorm < vmin	= vmin
	 	| vNotNorm > vmax	= vmax
		| otherwise		= vNotNorm
	
	dv	= vmax - vmin	

	result	| v < vmin + 0.25 * dv
		= ( 0
		  , 4 * (v - vmin) / dv
		  , 1.0)
		
		| v < vmin + 0.5 * dv
		= ( 0
		  , 1.0
		  , 1 + 4 * (vmin + 0.25 * dv - v) / dv)
		
		| v < vmin + 0.75 * dv
		= ( 4 * (v - vmin - 0.5 * dv) / dv
		  , 1.0
		  , 0.0)
		
		| otherwise
		= ( 1.0
		  , 1 + 4 * (vmin + 0.75 * dv - v) / dv
		  , 0)
		
  in makeColorFromTriple result


rampColor :: Float -> Float -> Float -> Color
rampColor vmin vmax vNotNorm
 = let	
	v	| vNotNorm < vmin	= vmin
	 	| vNotNorm > vmax	= vmax
		| otherwise		= vNotNorm
	
	dv	= vmax - vmin	

	result	= (0,0.1+(v-vmin)/dv,0.35+0.5*(v-vmin)/dv)
		
  in makeColorFromTriple result
