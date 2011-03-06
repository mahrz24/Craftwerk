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

import Craftwerk.Core.Driver.Generic

import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo (Matrix)
import qualified Graphics.Rendering.Cairo.Matrix as Matrix

import Control.Monad
import Control.Monad.Reader

data Context = Context { styleP :: StyleProperties
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
      Rotate r    -> Cairo.rotate (radians $ float2Double r)
      Scale p     -> fnC Cairo.scale p
      Translate p -> fnC Cairo.translate p
  figureToRenderContextWithStyle a 
  lift $ Cairo.restore
  
figureToRenderContextWithStyle (Canvas t a) = 
  local (\c -> c { strokeMatrix = transformationMatrix t (strokeMatrix c)
                 }) $ (figureToRenderContextWithStyle (Transform t a))

figureToRenderContextWithStyle (Composition a) =
  mapM_ figureToRenderContextWithStyle a

figureToRenderContextWithStyle (Path a) = ask >>= \c -> lift $ do
  let sp = getProperty $ styleP c
  when (sp fill ) (do cairoSetColor (sp fillColor)
                      cairoPath a sp
                      Cairo.fill)
  when (sp stroke) (do cairoSetColor (sp lineColor)
                       cairoSetLineJoin (sp lineJoin) 
                       cairoSetLineCap (sp lineCap)
                       Cairo.setMiterLimit (float2Double $ sp miterLimit)
                       Cairo.setDash (map float2Double $ sp dashes) 
                         (float2Double $ sp dashPhase)
                       cairoPath a sp
                       Cairo.setLineWidth (float2Double $ sp lineWidth)
                       Cairo.save
                       Cairo.setMatrix (strokeMatrix c)
                       Cairo.stroke
                       Cairo.restore)
    
figureToRenderContextWithStyle (Text a) = lift $ Cairo.textPath a >> Cairo.fill

figureToRenderContextWithStyle other = 
  figureToRenderContextWithStyle (genericFigure other)

-- Helper functions

pointConvert :: (Float, Float) -> (Double , Double)
pointConvert (a,b) = (float2Double a, float2Double b)

fnC :: (Double -> Double -> c) -> (Float, Float) -> c
fnC f = (uncurry f) . pointConvert

cairoSetColor (RGBA r g b a) =
  Cairo.setSourceRGB (float2Double r) (float2Double g) (float2Double b)
  
cairoSetLineJoin lj = 
  Cairo.setLineJoin (case lj of
                        JoinRound -> Cairo.LineJoinRound
                        JoinBevel ->  Cairo.LineJoinBevel
                        JoinMiter ->  Cairo.LineJoinMiter)


cairoSetLineCap lc = 
  Cairo.setLineCap (case lc of
                       CapRect -> Cairo.LineCapSquare
                       CapButt ->  Cairo.LineCapButt
                       CapRound ->  Cairo.LineCapRound)

cairoPath a sp = do sequence_ (map cairoSegment a) 
                    when (sp closePath) (Cairo.closePath)
                    
cairoSegment (MoveTo p) = (fnC Cairo.moveTo) p
cairoSegment (LineSegment p) = (fnC Cairo.lineTo) p
cairoSegment (ArcSegment (x,y) sa ea r) = 
  if sa > ea then
    Cairo.arcNegative (float2Double $ x-r*cos(radians sa)) (float2Double $ y-r*sin(radians sa)) 
    (float2Double r) (radians $ float2Double (sa)) (radians $ float2Double (ea))
  else
    Cairo.arc (float2Double $ x-r*cos(radians sa)) (float2Double $ y-r*sin(radians sa)) 
    (float2Double r) (radians $ float2Double (sa)) (radians $ float2Double (ea))
cairoSegment (CurveSegment (px,py) (c1x,c1y) (c2x,c2y)) = 
  Cairo.curveTo 
  (float2Double c1x) 
  (float2Double c1y) 
  (float2Double c2x) 
  (float2Double c2y) 
  (float2Double px) 
  (float2Double py) 
  
                    
radians :: (Floating a) => a -> a
radians n = n / (360 / (2 * pi))

transformationMatrix t m = case t of
  Rotate r    -> 
    Matrix.rotate (radians $ float2Double r) m
  Scale p     -> fnC Matrix.scale p m
  Translate p -> fnC Matrix.translate p m

