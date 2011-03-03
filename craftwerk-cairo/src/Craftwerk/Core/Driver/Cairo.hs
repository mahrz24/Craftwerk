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
import Graphics.Rendering.Cairo (Matrix)

import Control.Monad
import Control.Monad.Reader

data Context = Context { style :: StyleProperties
                       , strokeMatrix :: Matrix
                       }

type Render a = ReaderT Context Cairo.Render a

-- | Render a Craftwerk 'Figure' within a cairo render context
figureToRenderContext :: Figure -> Cairo.Render ()
figureToRenderContext f = do
  strokeMatrix <- Cairo.getMatrix
  runReaderT (figureToRenderContextWithStyle f) $
    Context { styleP = defaultStyle
            , strokeMatrix = strokeMatrix
            }

figureToRenderContextWithStyle Blank = return ()
figureToRenderContextWithStyle (Style ns a) =
  local (\c -> c { styleP = mergeProperties (styleP c) ns}) $
  figureToRenderContextWithStyle a

figureToRenderContextWithStyle (Transform t a) = do
  lift $ Cairo.save >>
    case t of
      Rotate r    -> Cairo.rotate (float2Double r)
      Scale p     -> fnC Cairo.scale p
      Translate p -> fnC Cairo.translate p
  figureToRenderContextWithStyle a 
  lift $ Cairo.restore

figureToRenderContextWithStyle (Composition a) =
  mapM_ figureToRenderContextWithStyle a

figureToRenderContextWithStyle (Line a) = ask >>= \c -> lift $ do
  let sp = getProperty $ styleP c
  when (sp fill ) (do cairoSetColor (sp fillColor)
                      cairoPath a sp
                      Cairo.fill)
  when (sp stroke) (do cairoSetColor (sp lineColor)
                       cairoPath a sp
                       Cairo.setLineWidth (float2Double $ sp lineWidth)
                       Cairo.save
                       Cairo.setMatrix (strokeMatrix c)
                       Cairo.stroke
                       Cairo.restore)


figureToRenderContextWithStyle (Text a) = lift $ Cairo.textPath a >> Cairo.fill

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
