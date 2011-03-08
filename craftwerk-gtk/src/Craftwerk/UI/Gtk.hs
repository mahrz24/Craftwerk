-- |
-- Module      :  Craftwerk.UI.Gtk
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
--

module Craftwerk.UI.Gtk (
  -- * Display figures
  displayRender,
  displayMultiple,
  renderFigure,
  renderWindow,
  value,
  choice,
  choices,
  isSet,
  Option(..)
  ) where

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo

import Craftwerk.Core.Driver.Cairo
import Craftwerk.Core.Driver.Tikz
import Craftwerk.Core.Figure
--import qualified Craftwerk.Core.Color

import Data.IORef
import qualified Data.Map as Map
import Data.List

import Control.Monad

import Text.Printf

import GHC.Float

data State = State { zoomFactor :: Double 
                   , currentContext :: String
                   , curOptions :: Map.Map String Option
                   }
                                                      
data Option = NumberOption Float
            | RangeOption Float Float Float Float
            | BoolOption Bool
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

data RenderContext = 
  RenderContext { cairo :: (Map.Map String Option -> Float -> Float -> Cairo.Render()) 
                , tikz :: (Map.Map String Option -> String) }
  
renderFigure :: Float -> 
                Float -> 
                (Map.Map String Option -> Figure) -> 
                RenderContext
renderFigure w h f  = 
  RenderContext r (figureToTikzPicture . f)
  where r op wx hx = figureToRenderContext $ scale (wx/w, -hx/h) $ translate (0,-h) $ (f op)
  
-- | Display a render context in a Gtk window, starts the Gtk main loop. 
-- The first argument gets the size of the window passed.
displayRender :: [(String, Option)] -> RenderContext -> IO ()
displayRender opt r = do
  initGUI
  window <- renderWindow opt $ [("Render",r)] 
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI
  
-- | Display multiple render contexts in a Gtk window, starts the Gtk main loop. 
-- The first argument gets the size of the window passed.
displayMultiple :: [(String, Option)] -> [(String, RenderContext)] -> IO ()
displayMultiple opt rcs = do
  initGUI
  window <- renderWindow opt rcs  
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI


renderWindow :: [(String, Option)] -> [(String, RenderContext)] -> IO Window
renderWindow opt ctxs = do  
  let rcs = Map.fromList ctxs
  window <- windowNew
  set window [windowTitle := "Menus and Toolbars",
              windowDefaultWidth := 420, windowDefaultHeight := 450]

  let firstContext = fst $ head ctxs

  -- Initialize the state
  stateRef <- newIORef (State { zoomFactor = 1.0 
                              , currentContext = firstContext
                              , curOptions = Map.fromList opt })
  
  -- The box layout
  box <- vBoxNew False 0
  containerAdd window box

  -- Init menubar and toolbar
  fma <- actionNew "FMA" "File" Nothing Nothing
  hma <- actionNew "HMA" "Help" Nothing Nothing

  expp <- actionNew "EXPP" "Export as PDF..."     (Just "Export as PDF") (Just stockConvert)
  expt <- actionNew "EXPT" "Export as TikZ..."    (Just "Export as TikZ") (Just stockDnd)
  exia <- actionNew "EXIA" "Exit"    (Just "Exit") (Just stockQuit)   
  zooi <- actionNew "ZOOI" "Zoom in"  (Just "Zoom in") (Just stockZoomIn)
  zooo <- actionNew "ZOOO" "Zoom out"  (Just "Zoom out") (Just stockZoomOut)
  zoof <- actionNew "ZOOF" "Zoom to fit"  (Just "Zoom to fit") (Just stockZoomFit)
  next <- actionNew "NEXT" "Next"  (Just "Next") (Just stockMediaNext)
  prev <- actionNew "PREV" "Previous"  (Just "Previous") (Just stockMediaPrevious)  
  hlpa <- actionNew "HLPA" "Help"  (Just "Help") (Just stockHelp)

  agr <- actionGroupNew "AGR"
  np <- actionGroupNew "NP"  
  
  mapM_ (actionGroupAddAction agr) [fma, hma]
  mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) 
    [expp,expt,zooi,zooo,zoof,hlpa]

  actionGroupAddActionWithAccel agr exia (Just "<Control>e")
  
  actionGroupAddActionWithAccel np next Nothing
  actionGroupAddActionWithAccel np prev Nothing
  
  when ((length ctxs) <= 1) (actionGroupSetSensitive np False)

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
  
  -- Create the drawing area, options need to update it
  canvas <- drawingAreaNew
  
  -- Create a hbox for options and drawing
  hpane <- hPanedNew
  
  --hbox <- hBoxNew False 0
  boxPackStart box hpane PackGrow 0
  
  sidebox <- vBoxNew False 0
  --boxPackStart hbox sidebox PackNatural 0
  containerAdd hpane sidebox

  -- Create the label and option widgets
  opt <- optionToUI canvas opt stateRef
  
  label <- labelNew (Just $ firstContext)
  boxPackStart sidebox label PackNatural 10

  boxPackStart sidebox opt PackNatural 10
  
  
  -- The display widgets
  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
  -- boxPackStart hbox scrwin PackGrow 0
  containerAdd hpane scrwin

  oframe <- aspectFrameNew 0.5 0.5 (Just 1)
  
  frameSetShadowType oframe ShadowNone

  fixed <- fixedNew
  frame <- aspectFrameNew 0.5 0.5 (Just 1)
  
  fixedPut fixed frame (0,0)  
  containerAdd oframe fixed

  scrolledWindowAddWithViewport scrwin oframe


  containerAdd frame canvas
  widgetSetSizeRequest canvas 400 400
    
  -- Show the window  
  widgetShowAll window 
  
  -- Draw actions
  onExpose canvas (\x ->  
                    do (w,h) <- widgetGetSize canvas
                       drawin <- widgetGetDrawWindow canvas
                       state <- readIORef stateRef
                       let f = cairo (rcs Map.! (currentContext state))
                       renderWithDrawable drawin 
                         (do Cairo.setSourceRGB 1.0 1.0 1.0
                             let dw = (fromIntegral w)
                                 dh = (fromIntegral h)
                             Cairo.moveTo 0 0
                             Cairo.lineTo 0 dw
                             Cairo.lineTo dw dh
                             Cairo.lineTo dh 0
                             Cairo.closePath
                             Cairo.fill
                             f (curOptions state) (fromIntegral w)  (fromIntegral h))
                       return True)
    
  -- Menu event actions
  onActionActivate exia (widgetDestroy window)
  
  onActionActivate zooi 
    (do state <- readIORef stateRef
        let zf = 2 * (zoomFactor state)
        writeIORef stateRef (state { zoomFactor = zf})
        resizeFrame canvas stateRef)
    
  onActionActivate zooo 
    (do state <- readIORef stateRef
        let zf = 0.5 * (zoomFactor state) 
        writeIORef stateRef (state { zoomFactor = zf})
        resizeFrame canvas stateRef)
  
  onActionActivate zoof 
    (do state <- readIORef stateRef
        let zf = 1
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
                                         let f = cairo (rcs Map.! (currentContext state))
                                         (Cairo.withPDFSurface path 
                                          (realToFrac 500) 
                                          (realToFrac 500) 
                                          (\s -> Cairo.renderWith s (f (curOptions state) 500 500)))
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
                                         let t = tikz (rcs Map.! (currentContext state)) 
                                         writeFile path (t (curOptions state))
          ResponseDeleteEvent -> return ()
        widgetDestroy fchdal)
    
  onActionActivate next
    (do state <- readIORef stateRef
        let cur = currentContext state
        let maybeidx = findIndex (\(a,b) -> a == cur) ctxs
        writeIORef stateRef 
          (state { currentContext = case maybeidx of 
                      Nothing -> fst $ head ctxs
                      Just idx -> if (idx+1) >= (length ctxs) then
                                    cur
                                  else (fst $ ctxs !! (idx + 1)) })
        nstate <- readIORef stateRef
        labelSetText label (currentContext nstate)
        widgetQueueDraw canvas)

  onActionActivate prev
    (do state <- readIORef stateRef
        let cur = currentContext state
        let maybeidx = findIndex (\(a,b) -> a == cur) ctxs
        writeIORef stateRef 
          (state { currentContext = case maybeidx of 
                      Nothing -> fst $ head ctxs
                      Just idx -> if (idx-1) < 0 then
                                    cur
                                  else (fst $ ctxs !! (idx - 1)) })
        nstate <- readIORef stateRef
        labelSetText label (currentContext nstate)
        widgetQueueDraw canvas)

  return window
  where resizeFrame canvas stateRef = 
          do state <- readIORef stateRef
             let zf = (zoomFactor state)
             widgetSetSizeRequest canvas (ceiling $ 400*zf) (ceiling $ 400*zf)        
             
optionToUI :: DrawingArea -> [(String,Option)] -> IORef (State) -> IO VBox
optionToUI canvas opt stateRef = do
    box <- vBoxNew False 0
    sep     <- hSeparatorNew
    boxPackStart box sep PackNatural 8
    label1 <- labelNew (Just "Options:")
    boxPackStart box label1 PackNatural 5
    sep2     <- hSeparatorNew
    boxPackStart box sep2 PackNatural 0    
    mapM_ (createOption canvas stateRef box) $ opt
    return box
    
createOption :: DrawingArea -> IORef (State) -> VBox -> (String,Option) -> IO ()
createOption canvas stateRef box (lbl, opt) = 
  do hbox <- hBoxNew False 0 
     boxPackStart box hbox PackNatural 5
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
         do adj <- adjustmentNew (float2Double $ def) (float2Double $ min)
                   (float2Double $ max) (float2Double $ step) 1.0 0.0
            scl <- hScaleNew adj
            boxPackStart hbox scl PackGrow 10
            onValueChanged adj (
              do state <- readIORef stateRef 
                 val <- adjustmentGetValue adj
                 writeIORef stateRef 
                   (state { curOptions = 
                               Map.update (const $ Just $ NumberOption (double2Float val)) 
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
         do cb <- comboBoxNewText
            mapM_ (comboBoxAppendText cb) choices
            comboBoxSetActive cb def
            boxPackStart hbox cb PackGrow 10
            on cb changed (
              do state <- readIORef stateRef 
                 val <- comboBoxGetActive cb
                 let c = [] --choices $ (curOptions state) Map.! lbl
                 writeIORef stateRef 
                   (state { curOptions = 
                               Map.update (const $ Just $ ChoiceOption c val) 
                               lbl 
                               (curOptions state)
                          })
                 widgetQueueDraw canvas
                 return ())
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
\            <toolitem action=\"PREV\" />\
\            <toolitem action=\"NEXT\" />\
\            <separator />\
\            <toolitem action=\"EXPP\" />\
\            <toolitem action=\"EXPT\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"

