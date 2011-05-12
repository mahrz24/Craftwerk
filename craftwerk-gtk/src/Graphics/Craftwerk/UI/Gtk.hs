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
  , RenderContext(..)

    -- * Display figures
  , renderFigure
  , displayRender
  , renderWindow

    -- * Option values
  , value
  , choice
  , choices
  , isSet

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
import Control.Monad.Trans

import Text.Printf

data State = State { zoomFactor :: Double
                   , curOptions :: Map.Map String Option
                   }

baseSize :: Int
baseSize = 400

-- | Describing options for a user interface.

data Option = NumberOption Double
              -- | A range: min, max, step and initial value.
            | RangeOption Double Double Double Double
            | BoolOption Bool
              -- | An option of choices with a default selection.
            | ChoiceOption [String] Int

value (NumberOption v) = v
value (RangeOption _ _ _ v) = v
value _ = 0.0

choice (ChoiceOption _ i) = i
choice _ = 0

choices (ChoiceOption s _) = s
choices _ = []

isSet (BoolOption b) = b
isSet _ = False

-- | Combined cairo and tikz rendering functions depending on the options.
data RenderContext =
  RenderContext { cairo :: Map.Map String Option -> Double -> Double -> IO (Cairo.Render())
                , tikz :: Map.Map String Option -> IO String
                , ctxWidth :: Double
                , ctxHeight :: Double }

heightInRatio :: RenderContext -> Int -> Int
heightInRatio rc w = 
    let aspectRatio = (ctxHeight rc)/(ctxWidth rc)
    in round $ (fromIntegral w) * aspectRatio

-- | Renders an 'IO Figure' into a render context with the given dimensions.
renderFigure :: Double -- ^ Width of the coordinate system of the GTK widget
                -> Double -- ^ Height of the coordinate system of the GTK widget
                -> (Map.Map String Option -> IO Figure) -- ^ The render function
                -> RenderContext
renderFigure w h f  = RenderContext r (liftM figureToTikzPicture . f) w h
  where r op wx hx = liftM (s wx hx) (f op)
        s wx hx = figureToRenderContext . scale (wx/w, -hx/h) . translate (0,-h)

-- | Display a render context in a Gtk window, starts the Gtk main loop.
-- The first argument contains a list of named options whose UI values
-- are passed to the render context.
displayRender :: [(String, Option)] -> RenderContext -> IO ()
displayRender opt r = do
  initGUI
  window <- renderWindow opt "Render View" r
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

-- | Same as 'displayMultiple' except that the Gtk main loop is not started or
-- initialized. The window is not visible upon return.
renderWindow :: [(String, Option)] -> String ->  RenderContext -> IO Window
renderWindow opt title ctx = do
  window <- windowNew
  set window [windowTitle := title,
              windowDefaultWidth := 640, windowDefaultHeight := 480]

  -- Initialize the state
  stateRef <- newIORef State { zoomFactor = 1.0
                             , curOptions = Map.fromList opt }

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

  canvas <- drawingAreaNew

  align <- alignmentNew 0.5 0.5 0 0
  containerAdd align canvas

  scrolledWindowAddWithViewport scrwinR align

  widgetSetSizeRequest canvas baseSize (heightInRatio ctx baseSize)

  -- Setup the pane
  frameL <- frameNew
  frameSetShadowType frameL ShadowNone
  containerAdd frameL scrwinL

  frameR <- frameNew
  frameSetShadowType frameR ShadowNone
  containerAdd frameR scrwinR

  containerAdd hpane frameL
  containerAdd hpane frameR
  panedSetPosition hpane 150

  -- === Option ui === --
  -- Create the label and option widgets
  opt <- optionToUI canvas opt stateRef
  boxPackStart sidebox opt PackGrow 10

  -- Show the window
  widgetShowAll window

  -- Draw actions
  onExpose canvas (\x ->
                    do (w,h) <- widgetGetSize canvas
                       drawin <- widgetGetDrawWindow canvas
                       state <- readIORef stateRef
                       let f = cairo ctx
                       fig <- f (curOptions state) (fromIntegral w)  (fromIntegral h)
                       renderWithDrawable drawin
                         (do Cairo.setSourceRGB 1.0 1.0 1.0
                             let dw = (fromIntegral w)
                                 dh = (fromIntegral h)
                             Cairo.moveTo 0 0
                             Cairo.lineTo 0 dh
                             Cairo.lineTo dw dh
                             Cairo.lineTo dw 0
                             Cairo.closePath
                             Cairo.fill
                             fig)
                       return True)

  -- Menu event actions
  onActionActivate exia (widgetDestroy window)

  onActionActivate zooi
    (do state <- readIORef stateRef
        let zf = 2 * zoomFactor state
        writeIORef stateRef (state { zoomFactor = zf})
        resizeFrame canvas stateRef)

  onActionActivate zooo
    (do state <- readIORef stateRef
        let zf = 0.5 * zoomFactor state
        writeIORef stateRef (state { zoomFactor = zf})
        resizeFrame canvas stateRef)

  onActionActivate zoof
    (do state <- readIORef stateRef
        (w,h) <- widgetGetSize scrwinR
        let zw = (fromIntegral w-10)/(fromIntegral baseSize) 
        let zh = (fromIntegral h-10)/(fromIntegral (heightInRatio ctx baseSize)) 
        let zf = min zw zh
        writeIORef stateRef (state { zoomFactor = zf})
        resizeFrame canvas stateRef)

  onActionActivate expp
    (do fchdal <- fileChooserDialogNew (Just "Export As PDF...") Nothing
                  FileChooserActionSave
                                     [("Cancel", ResponseCancel),
                                      ("Export", ResponseAccept)]
        fileChooserSetDoOverwriteConfirmation fchdal True
        widgetShow fchdal
        response <- dialogRun fchdal
        case response of
          ResponseCancel -> return ()
          ResponseAccept -> do nwf <- fileChooserGetFilename fchdal
                               case nwf of
                                    Nothing -> return ()
                                    Just path ->
                                      do state <- readIORef stateRef
                                         let f = cairo ctx
                                             w = (zoomFactor state)*
                                                 (fromIntegral baseSize)
                                             h =  fromIntegral (heightInRatio ctx $ round w)
                                         fig <- f (curOptions state) w h
                                         (Cairo.withPDFSurface path
                                          (realToFrac w)
                                          (realToFrac h)
                                          (`Cairo.renderWith` fig))
          ResponseDeleteEvent -> return ()
        widgetDestroy fchdal)

  onActionActivate expt
    (do fchdal <- fileChooserDialogNew (Just "Export As TikZ...") Nothing
                  FileChooserActionSave
                                     [("Cancel", ResponseCancel),
                                      ("Export", ResponseAccept)]
        fileChooserSetDoOverwriteConfirmation fchdal True
        widgetShow fchdal
        response <- dialogRun fchdal
        case response of
          ResponseCancel -> return ()
          ResponseAccept -> do nwf <- fileChooserGetFilename fchdal
                               case nwf of
                                    Nothing -> return ()
                                    Just path ->
                                      do state <- readIORef stateRef
                                         let t = tikz ctx
                                         fig <- (t (curOptions state))
                                         writeFile path fig
          ResponseDeleteEvent -> return ()
        widgetDestroy fchdal)

  return window
  where resizeFrame canvas stateRef =
          do state <- readIORef stateRef
             let zf = (zoomFactor state)
                 w = ceiling $ (fromIntegral baseSize)*zf
             widgetSetSizeRequest canvas w (heightInRatio ctx w)

optionToUI :: DrawingArea -> [(String,Option)] -> IORef State -> IO VBox
optionToUI canvas opt stateRef = do
    box <- vBoxNew False 0
    label1 <- labelNew (Just "Options:")
    boxPackStart box label1 PackNatural 5
    sep2     <- hSeparatorNew
    boxPackStart box sep2 PackNatural 0
    mapM_ (createOption canvas stateRef box) opt
    return box

createOption :: DrawingArea -> IORef State -> VBox -> (String,Option) -> IO ()
createOption canvas stateRef box (lbl, opt) =
  do hbox <- hBoxNew False 0

     case opt of
       ChoiceOption _ _ -> boxPackStart box hbox PackGrow 5
       _ -> do boxPackStart box hbox PackNatural 5
               label <- labelNew (Just lbl)
               boxPackStart hbox label PackNatural 10
     case opt of
       NumberOption def ->
         do field <- entryNew
            entrySetText field $ printf "%f" def
            boxPackStart hbox field PackGrow 10
            onEntryActivate field (
              do state <- readIORef stateRef
                 txt <- entryGetText field
                 writeIORef stateRef
                   (state { curOptions =
                               Map.update (const $ Just $ NumberOption (read txt))
                               lbl
                               (curOptions state)
                          })
                 widgetQueueDraw canvas
                 return ())
            return ()
       RangeOption min max step def ->
         do adj <- adjustmentNew def min max  step (step*10) 0.0
            scl <- spinButtonNew adj 0.5 4
            boxPackStart hbox scl PackGrow 10
            onValueChanged adj (
              do state <- readIORef stateRef
                 val <- adjustmentGetValue adj
                 writeIORef stateRef
                   (state { curOptions =
                               Map.update (const $ Just $ NumberOption val)
                               lbl
                               (curOptions state)
                          })
                 widgetQueueDraw canvas
                 return ())
            return ()
       BoolOption def ->
         do btn <- checkButtonNew
            toggleButtonSetActive btn def
            boxPackStart hbox btn PackNatural 10
            onToggled btn (
              do state <- readIORef stateRef
                 val <- toggleButtonGetActive btn
                 writeIORef stateRef
                   (state { curOptions =
                               Map.update (const $ Just $ BoolOption val)
                               lbl
                               (curOptions state)
                          })
                 widgetQueueDraw canvas
                 return ())
            return ()
       ChoiceOption choices def ->
         do list <- listStoreNew choices
            treeview <- treeViewNewWithModel list

            tvc <- treeViewColumnNew
            treeViewColumnSetTitle tvc lbl

            renderer <- cellRendererTextNew
            cellLayoutPackStart tvc renderer False
            cellLayoutSetAttributes tvc renderer list
              (\ind -> [cellText := ind])
            treeViewAppendColumn treeview tvc

            tree <- treeViewGetSelection treeview
            treeSelectionSetMode tree SelectionSingle
            treeSelectionSelectPath tree [0]

            scrwin <- scrolledWindowNew Nothing Nothing
            scrolledWindowSetPolicy scrwin PolicyNever PolicyAutomatic
            containerAdd scrwin treeview

            frame <- frameNew
            containerAdd frame scrwin

            boxPackStart hbox frame PackGrow 10

            onSelectionChanged tree (
              do sel <- treeSelectionGetSelectedRows tree
                 state <- readIORef stateRef
                 let val = head $ head sel
                 let c = [] --choices $ (curOptions state) Map.! lbl
                 writeIORef stateRef
                   (state { curOptions =
                               Map.update (const $ Just $ ChoiceOption c val)
                               lbl
                               (curOptions state)
                          })
                 widgetQueueDraw canvas
                 return ()
              )


            return ()

     sep     <- hSeparatorNew
     boxPackStart box sep PackNatural 0



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

