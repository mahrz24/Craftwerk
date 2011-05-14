-- |
-- Module      :  Graphics.Craftwerk.UI.RenderContext
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Graphics.Craftwerk.UI.RenderContext 
    ( RenderContext(..)
    -- * Display figures
    , renderFigure
    , saveContextAsPDF
    , saveContextAsTikZ
    , heightForWidth
    , widthForHeight
    ) where

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Craftwerk.Core.Driver.Cairo
import Graphics.Craftwerk.Core.Driver.Tikz
import Graphics.Craftwerk.Core.Figure

import Data.IORef
import qualified Data.Map as Map
import Data.List

import Control.Monad

-- | Combined cairo and tikz rendering functions depending 
-- on arbitrary parameters.
data RenderContext =
  RenderContext { cairo :: Double -> Double -> IO (Cairo.Render())
                , tikz :: IO String
                , ctxWidth :: Double
                , ctxHeight :: Double }

-- | Renders an 'IO Figure' into a render context with the given dimensions.
renderFigure :: Double -- ^ Width of the coordinate system of the GTK widget
                -> Double -- ^ Height of the coordinate system of the GTK widget
                -> IO Figure -- ^ The render function taking an arbitrary parameter
                -> RenderContext
renderFigure w h f  = RenderContext r (liftM figureToTikzPicture f) w h
  where r wx hx = liftM (s wx hx) f
        s wx hx = figureToRenderContext . scale (wx/w, -hx/h) . translate (0,-h)

saveContextAsPDF :: RenderContext -> Double -> String -> IO ()
saveContextAsPDF ctx w filename =
    let f = cairo ctx
        h = fromIntegral (heightForWidth ctx $ round w)
    in do fig <- f w h
          (Cairo.withPDFSurface filename
           (realToFrac w)
           (realToFrac h)
           (`Cairo.renderWith` fig))

saveContextAsTikZ :: RenderContext -> Double -> String -> IO ()
saveContextAsTikZ ctx w filename =
    let h = fromIntegral (heightForWidth ctx $ round w)
    in do fig <- tikz ctx
          writeFile filename fig

heightForWidth :: RenderContext -> Int -> Int
heightForWidth rc w = 
    let aspectRatio = (ctxHeight rc)/(ctxWidth rc)
    in round $ (fromIntegral w) * aspectRatio

widthForHeight :: RenderContext -> Int -> Int
widthForHeight rc h = 
    let aspectRatio = (ctxWidth rc)/(ctxHeight rc)
    in round $ (fromIntegral h) * aspectRatio

