{-# LANGUAGE RankNTypes, TemplateHaskell #-}
-- |
-- Module      :  Graphics.Craftwerk.UI.Options
-- Copyright   :  (c) Malte Harder 2011
-- License     :  MIT
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
-- 

module Graphics.Craftwerk.UI.Options 
    ( choice
    , vRef
    , linkUI
    , renderF
    , OptionUI(..) )
    where 

import Graphics.UI.Gtk
import Graphics.Craftwerk.UI.RenderContext
import Graphics.Craftwerk.Core.Figure

import Control.Monad.Trans
import Control.Monad.Writer

import Data.IORef

import Language.Haskell.TH

import Debug.Trace

import Data.Maybe

-- | Describing options for a user interface.

vRef :: OptionWriter a -> IO a
vRef = undefined

linkUI :: ExpQ -> ExpQ
linkUI uic = do ast <- uic
                let binds = getBinds ast
                let n = filterDo ast binds
                return $ trace (show binds) n
             
getBinds :: Exp -> [Name]
getBinds (DoE stmts) = catMaybes $ map extractBindVar stmts 
getBinds x = [] :: [Name]

extractBindVar (BindS (VarP v) _) = Just v
extractBindVar _ = Nothing

filterDo :: Exp -> [Name] -> Exp
filterDo (DoE stmts) binds = (AppE (VarE (mkName "runWriterT")) (DoE (map (filterStatements binds) stmts)))
filterDo x _ = x

filterStatements :: [Name] -> Stmt -> Stmt
filterStatements binds (NoBindS (InfixE (Just (VarE v)) a b)) = 
    (NoBindS (AppE (VarE (mkName "return")) (AppE (VarE (mkName "return")) (VarE (mkName "blank")))))
filterStatements _ (BindS (VarP v) (InfixE (Just (VarE r)) a (Just b))) =
    (BindS (VarP v) b)
filterStatements _ x = x

renderF :: Figure -> IO Figure
renderF = return

type OptionF = WidgetClass widget => widget -> IO HBox
data OptionUI = OptionUI { interface :: OptionF }

option :: OptionF -> OptionUI
option ifc = OptionUI { interface = ifc }

type OptionWriter a = (WriterT [OptionUI] IO (IORef a))

choice :: String -> [String] -> OptionWriter String
choice lbl choices =
    do ref <- (liftIO . newIORef) (head choices)
       tell [(option (choiceUI lbl choices ref))]
       return ref

choiceUI :: WidgetClass widget => String 
         -> [String] 
         -> IORef String 
         -> widget
         -> IO HBox
choiceUI lbl choices ref updateWidget = 
    do hbox <- hBoxNew False 0
       
       label <- labelNew (Just lbl)
       boxPackStart hbox label PackNatural 10
       
       list <- listStoreNew choices
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

       onSelectionChanged tree $ do 
         sel <- treeSelectionGetSelectedRows tree
         state <- readIORef ref
         writeIORef ref (choices !! (head $ head sel))
         widgetQueueDraw updateWidget
         return ()

       return hbox