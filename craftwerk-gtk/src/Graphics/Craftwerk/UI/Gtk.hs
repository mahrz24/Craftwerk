{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
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
  -- * Data types
    Option(..)

    -- * Display figures
  , renderFigure
  , displayRender
  , renderWindow

  ) where

import Graphics.UI.Gtk hiding (Socket)
import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Craftwerk.Core.Driver.Cairo
import Graphics.Craftwerk.Core.Driver.Tikz
import Graphics.Craftwerk.Core.Figure

import Graphics.Craftwerk.UI.RenderContext

import Data.IORef
import qualified Data.Map as Map
import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer

import Text.Printf

-- Put the export functions into 
-- Graphics.UI.RenderContext
-- Write function that creates the canvas and returns an ioref to the state
-- Remove all options as they are now
-- Option is a widget + ioref

data State = State { canvasWidth :: Double }

-- | Describing options for a user interface.

type OptionUI = WidgetClass widget => widget -> IO HBox

data Socket a = Socket { interface_ :: OptionUI
                       , value :: IORef a }

class SocketC s  where
    interface :: s -> OptionUI

instance SocketC (Socket a) where
    interface s = interface_ s

data Option = forall a . SocketC a => MakeOption a

instance SocketC Option where
    interface (MakeOption a) = interface a

type Options = [Option]

packSocket :: SocketC a => a -> Option
packSocket = MakeOption


option :: OptionUI -> IORef a -> Option
option ifc ref = packSocket Socket { interface_ = ifc 
                                    , value = ref}

type OptionWriter a = IO (Writer [Option] (IORef a))

choice :: String -> [String] -> OptionWriter String
choice lbl choices =
    do ref <- newIORef (head choices)
       tell option (choiceUI lbl choices ref) ref
       return ref

choiceUI :: String -> [String] -> IORef String -> OptionUI
choiceUI = undefined 


-- | Display a render context in a Gtk window, starts the Gtk main loop.
-- The first argument contains a list of named options whose UI values
-- are passed to the render context.
displayRender :: Options -> RenderContext -> IO ()
displayRender opt r = do
  initGUI
  window <- renderWindow opt "Render View" r
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

-- | Same as 'displayMultiple' except that the Gtk main loop is not started or
-- initialized. The window is not visible upon return.
renderWindow :: Options -> String ->  RenderContext -> IO Window
renderWindow opt title ctx = do
  window <- windowNew
  set window [windowTitle := title,
              windowDefaultWidth := 640, windowDefaultHeight := 480]

  -- The box layout
  box <- vBoxNew False 0
  containerAdd window box

  -- Init menubar and toolbar
  fma <- actionNew "FMA" "File" Nothing Nothing
  hma <- actionNew "HMA" "Help" Nothing Nothing

  expp <- actionNew "EXPP" "Export as PDF..."     (Just "Export as PDF") (Just stockConvert)
  expt <- actionNew "EXPT" "Export as TikZ..."    (Just "Export as TikZ") (Just stockDnd)
  exia <- actionNew "EXIA" "Close"    (Just "Close") (Just stockQuit)
  zooi <- actionNew "ZOOI" "Zoom in"  (Just "Zoom in") (Just stockZoomIn)
  zooo <- actionNew "ZOOO" "Zoom out"  (Just "Zoom out") (Just stockZoomOut)
  zoof <- actionNew "ZOOF" "Zoom to fit"  (Just "Zoom to fit") (Just stockZoomFit)
  hlpa <- actionNew "HLPA" "Help"  (Just "Help") (Just stockHelp)

  agr <- actionGroupNew "AGR"
  np <- actionGroupNew "NP"

  mapM_ (actionGroupAddAction agr) [fma, hma]
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing)
    [expp,expt,zooi,zooo,zoof,hlpa]

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

  maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
  let toolbar = case maybeToolbar of
        (Just x) -> x
        Nothing -> error "Cannot get toolbar from string."
  boxPackStart box toolbar PackNatural 0

  -- Main interface

  -- === Splitpane === --
  hpane <- hPanedNew
  boxPackStart box hpane PackGrow 0

  -- === Status Bar === --
  statusBar <- statusbarNew
  boxPackStart box statusBar PackNatural 0

  -- === Left side of pane === --
  -- Placeholder filled later
  sidebox <- vBoxNew False 0  
  scrwinL <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwinL PolicyNever PolicyAutomatic
  scrolledWindowAddWithViewport scrwinL sidebox

  -- === Main canvas === ---

   -- The display widgets
  scrwinR <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwinR PolicyAutomatic PolicyAutomatic

  (canvas, stateRef) <- contextCanvas ctx

  align <- alignmentNew 0.5 0.5 0 0
  containerAdd align canvas

  scrolledWindowAddWithViewport scrwinR align

  widgetSetSizeRequest canvas 400 (heightForWidth ctx 400)

  -- Setup the pane
  frameL <- frameNew
  frameSetShadowType frameL ShadowNone
  containerAdd frameL scrwinL

  frameR <- frameNew
  frameSetShadowType frameR ShadowNone
  containerAdd frameR scrwinR

  containerAdd hpane frameL
  containerAdd hpane frameR
  panedSetPosition hpane 230

  -- === Option ui === --
  -- Create the label and option widgets
  optUI <- optionToUI canvas opt
  boxPackStart sidebox optUI PackGrow 10

  -- Show the window
  widgetShowAll window

  -- Menu event actions
  onActionActivate exia (widgetDestroy window)

  onActionActivate zooi
    (do state <- readIORef stateRef
        let zf = 1.5 * canvasWidth state
        writeIORef stateRef (state { canvasWidth = zf})
        resizeFrame canvas stateRef)

  onActionActivate zooo
    (do state <- readIORef stateRef
        let zf = 0.75 * canvasWidth state
        writeIORef stateRef (state { canvasWidth = zf})
        resizeFrame canvas stateRef)

  onActionActivate zoof 
                       (zoomCanvasToFit canvas scrwinR stateRef)
    
  onActionActivate expp 
                       (exportToFile "Export As PDF..." 
                                     saveContextAsPDF stateRef)

  onActionActivate expt 
                       (exportToFile "Export As TikZ..." 
                                     saveContextAsTikZ stateRef)

  return window

  where resizeFrame canvas stateRef =
            do state <- readIORef stateRef
               let w = ceiling (canvasWidth state)
               widgetSetSizeRequest canvas w (heightForWidth ctx w)
        zoomCanvasToFit canvas scrwinR stateRef =
            do state <- readIORef stateRef
               (w,h) <- widgetGetSize scrwinR
               let zf = min (fromIntegral w-10) (fromIntegral h-10)
               writeIORef stateRef (state { canvasWidth = zf})
               resizeFrame canvas stateRef
        exportToFile prompt exporter stateRef = 
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
                             do state <- readIORef stateRef
                                let w = (canvasWidth state)
                                exporter ctx w path
                 ResponseDeleteEvent -> return ()
               widgetDestroy fchdal


contextCanvas ctx =
    do canvas <- drawingAreaNew
       stateRef <- newIORef State { canvasWidth = 400 }
       onExpose canvas (drawFigure canvas stateRef)
       return (canvas,stateRef)
    where drawFigure canvas stateRef x =
              do (w,h) <- widgetGetSize canvas
                 drawin <- widgetGetDrawWindow canvas
                 state <- readIORef stateRef
                 let f = cairo ctx
                 fig <- f (fromIntegral w) (fromIntegral h)
                 renderWithDrawable drawin (renderFig fig w h)
                 return True
          renderFig fig w h =
              do Cairo.setSourceRGB 1.0 1.0 1.0
                 let dw = (fromIntegral w)
                     dh = (fromIntegral h)
                 Cairo.moveTo 0 0
                 Cairo.lineTo 0 dh
                 Cairo.lineTo dw dh
                 Cairo.lineTo dw 0
                 Cairo.closePath
                 Cairo.fill
                 fig

optionToUI :: DrawingArea -> Options -> IO VBox
optionToUI canvas opt = do
    box <- vBoxNew False 0
    label1 <- labelNew (Just "Options:")
    boxPackStart box label1 PackNatural 5
    mapM_ (\ow -> do hbox <- interface ow $ canvas
                     boxPackStart box hbox PackNatural 0) opt
    return box

-- createOption :: DrawingArea -> IORef State -> VBox -> (String,Option) -> IO ()
-- createOption canvas stateRef box (lbl, opt) =
--   do hbox <- hBoxNew False 0

--      case opt of
--        ChoiceOption _ _ -> boxPackStart box hbox PackGrow 5
--        _ -> do boxPackStart box hbox PackNatural 5
--                label <- labelNew (Just lbl)
--                boxPackStart hbox label PackNatural 10
--      case opt of
--        NumberOption def ->
--          do field <- entryNew
--             entrySetText field $ printf "%f" def
--             boxPackStart hbox field PackGrow 10
--             onEntryActivate field (
--               do state <- readIORef stateRef
--                  txt <- entryGetText field
--                  writeIORef stateRef
--                    (state { curOptions =
--                                Map.update (const $ Just $ NumberOption (read txt))
--                                lbl
--                                (curOptions state)
--                           })
--                  widgetQueueDraw canvas
--                  return ())
--             return ()
--        RangeOption min max step def ->
--          do adj <- adjustmentNew def min max  step (step*10) 0.0
--             scl <- spinButtonNew adj 0.5 4
--             boxPackStart hbox scl PackGrow 10
--             onValueChanged adj (
--               do state <- readIORef stateRef
--                  val <- adjustmentGetValue adj
--                  writeIORef stateRef
--                    (state { curOptions =
--                                Map.update (const $ Just $ NumberOption val)
--                                lbl
--                                (curOptions state)
--                           })
--                  widgetQueueDraw canvas
--                  return ())
--             return ()
--        BoolOption def ->
--          do btn <- checkButtonNew
--             toggleButtonSetActive btn def
--             boxPackStart hbox btn PackNatural 10
--             onToggled btn (
--               do state <- readIORef stateRef
--                  val <- toggleButtonGetActive btn
--                  writeIORef stateRef
--                    (state { curOptions =
--                                Map.update (const $ Just $ BoolOption val)
--                                lbl
--                                (curOptions state)
--                           })
--                  widgetQueueDraw canvas
--                  return ())
--             return ()
--        ChoiceOption choices def ->
--          do list <- listStoreNew choices
--             treeview <- treeViewNewWithModel list

--             tvc <- treeViewColumnNew
--             treeViewColumnSetTitle tvc lbl

--             renderer <- cellRendererTextNew
--             cellLayoutPackStart tvc renderer False
--             cellLayoutSetAttributes tvc renderer list
--               (\ind -> [cellText := ind])
--             treeViewAppendColumn treeview tvc

--             tree <- treeViewGetSelection treeview
--             treeSelectionSetMode tree SelectionSingle
--             treeSelectionSelectPath tree [0]

--             scrwin <- scrolledWindowNew Nothing Nothing
--             scrolledWindowSetPolicy scrwin PolicyNever PolicyAutomatic
--             containerAdd scrwin treeview

--             frame <- frameNew
--             containerAdd frame scrwin

--             boxPackStart hbox frame PackGrow 10

--             onSelectionChanged tree (
--               do sel <- treeSelectionGetSelectedRows tree
--                  state <- readIORef stateRef
--                  let val = head $ head sel
--                  let c = [] --choices $ (curOptions state) Map.! lbl
--                  writeIORef stateRef
--                    (state { curOptions =
--                                Map.update (const $ Just $ ChoiceOption c val)
--                                lbl
--                                (curOptions state)
--                           })
--                  widgetQueueDraw canvas
--                  return ()
--               )


--             return ()

--      sep     <- hSeparatorNew
--      boxPackStart box sep PackNatural 0



uiStd =  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"EXPP\" />\
\              <menuitem action=\"EXPT\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"ZOOI\" />\
\            <toolitem action=\"ZOOO\" />\
\            <toolitem action=\"ZOOF\" />\
\            <separator />\
\            <toolitem action=\"EXPP\" />\
\            <toolitem action=\"EXPT\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

