-- |
-- Module      :  Craftwerk.UI.Gtk
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.UI.Gtk (
  -- * Display figures
  displayRender
  ) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Craftwerk.Core.Driver.Cairo
import Craftwerk.Core.Figure
import Craftwerk.Core.Color

import GHC.Float

-- | Display a render context in a Gtk window, starts the Gtk main loop. 
-- The first argument gets the size of the window passed.
displayRender :: (Float -> Float -> Render ()) -> IO ()
displayRender f = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Figure Display",
              windowDefaultWidth := 300, windowDefaultHeight := 200,
              containerBorderWidth := 30 ]

  frame <- frameNew
  containerAdd window frame
  canvas <- drawingAreaNew
  containerAdd frame canvas
  
  widgetShowAll window 
  onExpose canvas (\x ->  
                    do (w,h) <- widgetGetSize canvas
                       drawin <- widgetGetDrawWindow canvas
                       renderWithDrawable drawin 
                         (f (fromIntegral w) (fromIntegral h))
                       return True)
    
  onDestroy window mainQuit
  mainGUI

