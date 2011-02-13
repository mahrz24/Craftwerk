-- |
-- Module      :  Craftwerk.Core.Driver.Cairo
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.Core.Driver.Cairo (
  -- * Cairo rendering
  figureToRenderContext
  )where

import GHC.Float

import Craftwerk.Core.Figure
import Craftwerk.Core.Color
import Craftwerk.Core.Style

import qualified Graphics.Rendering.Cairo as Cairo

import Control.Monad

-- | Render a Craftwerk 'Figure' within a cairo render context
figureToRenderContext :: Figure -> Cairo.Render ()
figureToRenderContext = figureToRenderContextWithStyle defaultStyle

figureToRenderContextWithStyle _ Blank = return ()
figureToRenderContextWithStyle s (Style ns a) =
  (figureToRenderContextWithStyle (mergeProperties s ns) a)

figureToRenderContextWithStyle s (Transform t a) = do
  Cairo.save
  case t of
    Rotate r    -> Cairo.rotate (float2Double r)
    Scale p     -> fnC Cairo.scale p
    Translate p -> fnC Cairo.translate p
  figureToRenderContextWithStyle s a
  Cairo.restore

figureToRenderContextWithStyle s (Composition a) =
  sequence_ (map (figureToRenderContextWithStyle s) a)

figureToRenderContextWithStyle s (Line a) = do
  let sp = getProperty s
  when (sp fill ) (do cairoSetColor (sp fillColor)
                      cairoPath a sp
                      Cairo.fill)
  when (sp stroke) (do cairoSetColor (sp lineColor)
                       cairoPath a sp
                       Cairo.setLineWidth (float2Double $ sp lineWidth )
                       Cairo.stroke)


figureToRenderContextWithStyle s (Text a) = Cairo.textPath a >> Cairo.fill

-- Helper functions

pointConvert :: (Float, Float) -> (Double , Double)
pointConvert (a,b) = (float2Double a, float2Double b)

fnC :: (Double -> Double -> c) -> (Float, Float) -> c
fnC f = (uncurry f) . pointConvert

cairoSetColor (RGBA r g b a) =
  Cairo.setSourceRGB (float2Double r) (float2Double g) (float2Double b)

cairoPath a sp = do (fnC Cairo.moveTo) (head a)
                    sequence_ (map (fnC Cairo.lineTo) a)
                    when (sp closePath) (Cairo.closePath)
