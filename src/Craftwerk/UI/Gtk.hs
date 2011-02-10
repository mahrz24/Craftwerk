module Craftwerk.UI.Gtk where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Craftwerk.Core.Driver.Cairo
import Craftwerk.Core.Figure
import Craftwerk.Core.Color

import Data.Word
import Data.Array.MArray

import Data.Vector

import GHC.Float


displayRender :: (Double -> Double -> Render ()) -> IO ()
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
  onExpose canvas (\x ->  do (w,h) <- widgetGetSize canvas
                             drawin <- widgetGetDrawWindow canvas
                             renderWithDrawable drawin 
                               (f (fromIntegral w) (fromIntegral h))
                             return True)
  
  onDestroy window mainQuit
  mainGUI

pictureToPixbuf :: Picture -> IO Pixbuf
pictureToPixbuf (Picture w h v) = do pixbuf <- pixbufNew ColorspaceRgb False 8 w h 
                                     pixData <- pixbufGetPixels pixbuf :: IO (PixbufData Int Word8)
                                     nChannels <- pixbufGetNChannels pixbuf
                                     rowstride <- pixbufGetRowstride pixbuf
                                     forM_ (fromList [0..(h-1)]) 
                                       (\y ->
                                         forM_ (fromList [0..(w-1)]) 
                                         (\x ->
                                           let (r,g,b) = getRGB (v Data.Vector.! (y*w+x))
                                               p = (y * rowstride + x * nChannels)
                                           in  writeArray pixData p (normC r) >>
                                               writeArray pixData (p+1) (normC g) >>
                                               writeArray pixData (p+2) (normC b)
                                         )
                                       )
                                     return pixbuf
  where normC v = fromIntegral $ float2Int $ v * 255

displayPixbuf :: (Double -> Double -> IO Pixbuf) -> IO ()
displayPixbuf f = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Figure Display",
              windowDefaultWidth := 300, windowDefaultHeight := 200,
              containerBorderWidth := 30 ]

  frame <- frameNew
  containerAdd window frame
  (w,h) <- widgetGetSize frame
  pixbuf <- (f (fromIntegral w) (fromIntegral h))
  image <- imageNewFromPixbuf pixbuf
  containerAdd frame image
  
  widgetShowAll window 
 
    
  onDestroy window mainQuit
  mainGUI