{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, TypeFamilies #-}
-- |
-- Module      :  Graphics.Craftwerk.UI.Gtk
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--
-- The UI functions of Craftwerk allow quick display of figures. They also
-- support the creation of a simple user interface to control parameters of
-- the figures that are displayed.

module Graphics.Craftwerk.UI.Gtk (
  
  module Graphics.Craftwerk.UI.TV
  ) where

import Graphics.Craftwerk.UI.TV

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Craftwerk.Core.Driver.Cairo
import Graphics.Craftwerk.Core

import Control.Applicative (liftA2,(<$>),(<*>),(<$))
import Control.Monad (when,join)

import qualified Data.Map as Map
import Data.IORef
import Data.Word

import Text.Printf

in1 = sliderIIn (0,5) 1
in2 = sliderIIn (0,5) 1

pinski :: GTV (Colour Double -> Int -> Figure)
pinski = tv 
         (oTitle "Sierpinski Triangle" $ 
          oLambda (iTitle "Color" defaultIn) (oLambda (iTitle "Iterations" in1) (figureOut 2.0 1.0)) ) tpair

tpair col i = colorStyle col $ iterations i $ triangle

colorStyle col = style newStyle { closePath = yes
                                , fillColor = Just col
                                , stroke = no
                                , fill = yes}

triangle = line [(0,0),(0,1),(1,0)]

iterations :: Int -> Figure -> Figure
iterations 0 f = f
iterations i f =
  let nf = iterations (i-1) f
  in scale (0.5,0.5) $ composition
     [
       translate (0,1) $ nf
     , translate (0,0) $ nf
     , translate (1,0) $ nf
     ]

-- Create an interface for GTV

runGTVInWindow :: String -> GTV a -> IO ()
runGTVInWindow name f = do
  initGUI
  window <- windowNew

  set window [windowTitle := name
             , windowDefaultWidth := 640
             , windowDefaultHeight := 480
             ]

  -- The box layout
  box <- vBoxNew False 0
  containerAdd window box

  -- Init menubar and toolbar
  fma <- actionNew "FMA" "File" Nothing Nothing
  hma <- actionNew "HMA" "Help" Nothing Nothing

  exia <- actionNew "EXIA" "Close"    (Just "Close") (Just stockQuit)
  hlpa <- actionNew "HLPA" "Help"  (Just "Help") (Just stockHelp)

  agr <- actionGroupNew "AGR"
  np <- actionGroupNew "NP"

  mapM_ (actionGroupAddAction agr) [fma, hma]
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing)
    [hlpa]

  actionGroupAddActionWithAccel agr exia (Just "<Control>e")

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiStd
  uiManagerInsertActionGroup ui agr 0
  uiManagerInsertActionGroup ui np 0

  maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
  let menubar = case maybeMenubar of
        (Just x) -> x
        Nothing -> error "Cannot get menubar from string."
  boxPackStart box menubar PackNatural 0


  -- === Splitpane === --
  wid <- runGTVInWidget f
  boxPackStart box wid PackGrow 0

  -- === Status Bar === --
  statusBar <- statusbarNew
  boxPackStart box statusBar PackNatural 0

    -- Menu event actions
  onActionActivate exia (widgetDestroy window)
  
  onDestroy window (mainQuit)
  
  widgetShowAll window
  mainGUI
  return ()
  
uiStd =  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\          </ui>"

instance (Ord a, Floating a) => DefaultIn MkI (Colour a) where defaultIn = colorIn black

getRed :: Fractional a => Color -> a
getRed (Color r g b) = (fromIntegral r) / (65536)

getGreen :: Fractional a => Color -> a
getGreen (Color r g b) = (fromIntegral g) / (65536)

getBlue :: Fractional a => Color -> a
getBlue (Color r g b) = (fromIntegral b) / (65536)

convertC c = sRGB (getRed c) (getGreen c) (getBlue c)

colorIn :: (Ord a, Floating a) => (Colour a) -> In (Colour a)
colorIn a0 = 
  primMkI $
  do oldRef <- newIORef a0
     w <- colorButtonNewWithColor (Color 0 0 0)
     let getter = 
           do c <- colorButtonGetColor w
              return $ convertC c
         install refresh = forget $ afterColorSet w
                               (do c <- colorButtonGetColor w
                                   changeTo (convertC c) 
                                   return ())
          where
            changeTo new =
              do old <- readIORef oldRef
                 when (old /= new) $
                      do refresh
                         writeIORef oldRef new
     return (toWidget w, getter, return (), install)

instance DefaultOut MkI MkO Figure where defaultOut = figureOut 1.0 1.0

figureOut :: Double -> Double -> Out Figure
figureOut cw ch = 
  primMkO $ 
  do 
    figureRef <- newIORef blank
    scrwinR <- scrolledWindowNew Nothing Nothing
   
   
    scrolledWindowSetPolicy scrwinR PolicyAutomatic PolicyAutomatic

    canvas <- drawingAreaNew
    
    let aspectRatio w = round $ (fromIntegral w) * ch/cw
    let aspectRatio' h = round $ (fromIntegral h) * cw/ch
    widgetSetSizeRequest canvas 400 (aspectRatio 400)
    
    align <- alignmentNew 0.5 0.5 0 0
    containerAdd align canvas
    
    scrolledWindowAddWithViewport scrwinR align

    btnbox <- hButtonBoxNew
    zibtn <- buttonNewFromStock stockZoomIn
    containerAdd btnbox zibtn
    
    onClicked zibtn (do
                        (w,h) <- widgetGetSize canvas
                        let nw = 2*w
                        widgetSetSizeRequest canvas nw (aspectRatio nw)
                    )
    
    zobtn <- buttonNewFromStock stockZoomOut
    containerAdd btnbox zobtn
    
    onClicked zobtn (do
                        (w,h) <- widgetGetSize canvas
                        let nw = round $ (fromIntegral w)/2
                        widgetSetSizeRequest canvas nw (aspectRatio nw)
                    )

    zfbtn <- buttonNewFromStock stockZoomFit
    containerAdd btnbox zfbtn

    onClicked zfbtn (do
                        (w,h) <- widgetGetSize scrwinR
                        let nw = w-10
                            nh = h-10
                        if (aspectRatio nw) < nh then
                           widgetSetSizeRequest canvas nw (aspectRatio nw)
                          else
                          widgetSetSizeRequest canvas (aspectRatio nh) nh
                    )

    pdfbtn <- buttonNewWithLabel "Export as PDF..."
    containerAdd btnbox pdfbtn
    
    onClicked pdfbtn 
      (exportToFile "Export as PDF" (saveFigureAsPDF $ toWidget canvas) figureRef)

    tikzbtn <- buttonNewWithLabel "Export as TikZ..."
    containerAdd btnbox tikzbtn

    onClicked tikzbtn 
      (exportToFile "Export as TikZ" saveFigureAsTikZ figureRef)

    vbox <- vBoxNew False 1
    containerSetBorderWidth vbox 5
    boxPackStart vbox btnbox PackNatural 0
    boxPackStart vbox scrwinR PackGrow 5
         
    let display figure = 
          do writeIORef figureRef figure
             widgetQueueDraw canvas
              
    onExpose canvas $ \_ ->
      do figure <- readIORef figureRef
         drawin <- widgetGetDrawWindow canvas
         (w,h) <- widgetGetSize canvas 
         renderWithDrawable drawin (renderFig figure w h)
         return True
          
    return (toWidget vbox, display, return ())
  where renderFig fig w h =
          do Cairo.setSourceRGB 1.0 1.0 1.0
             let dw = (fromIntegral w)
                 dh = (fromIntegral h)
             Cairo.moveTo 0 0
             Cairo.lineTo 0 dh
             Cairo.lineTo dw dh
             Cairo.lineTo dw 0
             Cairo.closePath
             Cairo.fill
             figureToRenderContext . scale (dw/cw, -dh/ch) . translate (0,-ch) $ fig 
        exportToFile prompt exporter figureRef = 
          do fchdal <- fileChooserDialogNew (Just prompt) Nothing
                       FileChooserActionSave
                       [("Cancel", ResponseCancel),
                        ("Export", ResponseAccept)]
             fileChooserSetDoOverwriteConfirmation fchdal True
             widgetShow fchdal
             response <- dialogRun fchdal
             case response of
               ResponseCancel -> 
                 return ()
               ResponseAccept -> 
                 do nwf <- fileChooserGetFilename fchdal
                    case nwf of
                      Nothing -> return ()
                      Just path ->
                        do fig <- readIORef figureRef
                           exporter cw ch fig path
               ResponseDeleteEvent -> return ()
             widgetDestroy fchdal

saveFigureAsPDF :: Widget -> Double -> Double -> Figure -> String -> IO ()
saveFigureAsPDF c cw ch f filename =
  do (w,h) <- widgetGetSize c
     let dw = (fromIntegral w)
         dh = (fromIntegral h)
     (Cairo.withPDFSurface filename
      (realToFrac dw)
      (realToFrac dh)
      (`Cairo.renderWith` 
       (figureToRenderContext . scale (dw/cw, -dh/ch) . translate (0,-ch) $ f)))

saveFigureAsTikZ :: Double -> Double -> Figure -> String -> IO ()
saveFigureAsTikZ w h f filename = 
  writeFile filename (figureToTikzPicture f)


