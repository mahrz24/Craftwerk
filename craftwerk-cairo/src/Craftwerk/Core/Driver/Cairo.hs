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
                       , noDecorations :: Bool
                       }

type Render a = ReaderT Context Cairo.Render a

-- | Render a Craftwerk 'Figure' within a cairo render context
figureToRenderContext :: Figure -> Cairo.Render ()
figureToRenderContext f = do
  strokeMatrix <- Cairo.getMatrix
  runReaderT (figureToRenderContextWithStyle f) $
    Context { styleP = defaultStyle
            , strokeMatrix = strokeMatrix
            , noDecorations = False
            }

figureToRenderContextWithStyle Blank = return ()
figureToRenderContextWithStyle (Style ns a) =
  local (\c -> c { styleP = mergeProperties (styleP c) ns}) $
  figureToRenderContextWithStyle a

figureToRenderContextWithStyle (Transform t a) = do
  lift $ Cairo.save >>
    case t of
      Rotate r    -> Cairo.rotate (radians r)
      Scale p     -> fnC Cairo.scale p
      Translate p -> fnC Cairo.translate p
  figureToRenderContextWithStyle a 
  lift $ Cairo.restore
  
figureToRenderContextWithStyle (Canvas t a) = 
  local (\c -> c { strokeMatrix = transformationMatrix t (strokeMatrix c)
                 }) $ (figureToRenderContextWithStyle (Transform t a))

figureToRenderContextWithStyle (Composition a) =
  mapM_ figureToRenderContextWithStyle a

figureToRenderContextWithStyle (NoDecorations a) = 
  local (\c -> c { noDecorations = True
                 }) $ (figureToRenderContextWithStyle a)

figureToRenderContextWithStyle (Path a) = ask >>= \c -> 
  let sp = getProperty $ styleP c
  in do lift $ do when (sp clip) (do cairoPath a sp
                                     Cairo.clip)   
                  when (sp fill && (not $ sp clip)) 
                    (do cairoSetColor (sp fillColor)
                        cairoPath a sp
                        Cairo.fill)
                  when (sp stroke && (not $ sp clip)) 
                    (do cairoSetColor (sp lineColor)
                        cairoSetLineJoin (sp lineJoin) 
                        cairoSetLineCap (sp lineCap)
                        Cairo.setMiterLimit  (sp miterLimit)
                        Cairo.setDash (sp dashes) (sp dashPhase)
                        cairoPath a sp
                        Cairo.setLineWidth (sp lineWidth)
                        Cairo.save
                        Cairo.setMatrix (strokeMatrix c)
                        Cairo.stroke 
                        Cairo.restore)
        -- Avoid running into a deadlock when rendering arrow tips
        when (not $ noDecorations c) 
          (do (figureToRenderContextWithStyle $ (arrowTipsForPath a (sp lineWidth) (sp arrowTips))))
  
    
figureToRenderContextWithStyle (Text a) = lift $ Cairo.textPath a >> Cairo.fill

figureToRenderContextWithStyle other = 
  figureToRenderContextWithStyle (genericFigure other)

-- Helper functions

fnC :: (Double -> Double -> c) -> (Double, Double) -> c
fnC f = uncurry f

cairoSetColor color =
  let rgb = toSRGB color
  in Cairo.setSourceRGB (channelRed rgb) 
     (channelGreen rgb) 
     (channelBlue rgb)

  
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
    Cairo.arcNegative (x-r*cos(radians sa)) (y-r*sin(radians sa)) 
     r (radians sa) (radians ea)
  else
    Cairo.arc (x-r*cos(radians sa)) (y-r*sin(radians sa)) 
     r (radians sa) (radians ea)
cairoSegment (CurveSegment (px,py) (c1x,c1y) (c2x,c2y)) = 
  Cairo.curveTo 
  c1x 
  c1y 
  c2x 
  c2y 
  px 
  py 
  
  
transformationMatrix t m = case t of
  Rotate r    -> 
    Matrix.rotate (radians r) m
  Scale p     -> fnC Matrix.scale p m
  Translate p -> fnC Matrix.translate p m

