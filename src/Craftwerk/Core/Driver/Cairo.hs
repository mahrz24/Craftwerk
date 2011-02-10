module Craftwerk.Core.Driver.Cairo where

import GHC.Float

import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import qualified Graphics.Rendering.Cairo as Cairo

import Control.Monad

pointConvert :: (Float, Float) -> (Double , Double)
pointConvert (a,b) = (float2Double a, float2Double b)

fnC :: (Double -> Double -> c) -> (Float, Float) -> c
fnC f = (uncurry f) . pointConvert

figureToRenderContext :: Figure -> Cairo.Render ()
figureToRenderContext = figureToRenderContextWithStyle [Stroke True]

figureToRenderContextWithStyle _ Blank = return ()
figureToRenderContextWithStyle s (Style ns a) = (figureToRenderContextWithStyle (mergeProperties s ns) a)
figureToRenderContextWithStyle s (Rotate r a) = Cairo.save >> Cairo.rotate (float2Double r) >> figureToRenderContextWithStyle s a >> Cairo.restore
figureToRenderContextWithStyle s (Scale p a) = Cairo.save >> (fnC Cairo.scale) p >> figureToRenderContextWithStyle s a >> Cairo.restore
figureToRenderContextWithStyle s (Translate p a) = Cairo.save >> (fnC Cairo.translate) p  >> figureToRenderContextWithStyle s a >> Cairo.restore
figureToRenderContextWithStyle s (Composition a) = sequence_ (map (figureToRenderContextWithStyle s) a)
figureToRenderContextWithStyle s (Line a) = do when (stroke s) (do 
                                                                   maybe (Cairo.setSourceRGB 0 0 0) (\(RGBA r g b a) -> Cairo.setSourceRGBA 
                                                                                                                        (float2Double r) 
                                                                                                                        (float2Double g) 
                                                                                                                        (float2Double b) 
                                                                                                                        (float2Double a)) (lineColor s)
                                                                   (fnC Cairo.moveTo) (head a) 
                                                                   sequence_ (map (fnC Cairo.lineTo) a)
                                                                   when (closePath s) (fnC Cairo.lineTo $ head a) 
                                                                   Cairo.setLineWidth (float2Double $ lineWidth s)
                                                                   Cairo.stroke)
                                               when (fill s) (do 
                                                                   maybe (Cairo.setSourceRGB 0 0 0) (\(RGBA r g b a) -> Cairo.setSourceRGBA 
                                                                                                                        (float2Double r) 
                                                                                                                        (float2Double g) 
                                                                                                                        (float2Double b) 
                                                                                                                        (float2Double a)) (fillColor s)
                                                                   (fnC Cairo.moveTo) (head a) 
                                                                   sequence_ (map (fnC Cairo.lineTo) a)
                                                                   when (closePath s) (fnC Cairo.lineTo $ head a) 
                                                                   Cairo.fill)
                                               
figureToRenderContextWithStyle s (Text a) = Cairo.textPath a >> Cairo.fill