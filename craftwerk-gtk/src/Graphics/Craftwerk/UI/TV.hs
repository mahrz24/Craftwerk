{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Craftwerk.UI.Gtk
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  Malte Harder <malte.harder@gmail.com>
-- Stability   :  experimental
--
-- Use tangible values with Craftwerk and Gtk, this is basically
-- the Interface.TV.Gtk2 module by Conal Elliot with some minor changes
-- 
-- Gtk-based GUIs in the TV (tangible value) framework
-- 
-- This variation eliminates mdo by having MkI' produce a consumer of
-- refresh actions rather than taking a refresh action as argument.
----------------------------------------------------------------------

module Graphics.Craftwerk.UI.TV
  ( -- * TV type specializations
    In, Out, GTV, gtv, runGTV, runGTVInWidget, runOut, runOutIO
    -- * UI primitives
  , primMkI, primMkO
  , R, sliderRIn, sliderIIn, clockIn
  , rateSliderIn, integralIn
  , fileNameIn
  , MkO, MkI
  , forget, forget2
  , module Interface.TV
  ) where

import Control.Applicative (liftA2,(<$>),(<*>),(<$))
import Control.Monad (when,join)
import Data.IORef
import Data.Maybe (fromMaybe)

import Data.Time (getCurrentTime,utctDayTime)

import Graphics.UI.Gtk hiding (Action)
import qualified Graphics.UI.Gtk as Gtk

-- From vector-space
import Data.VectorSpace

-- From TypeCompose
import Data.Title
import Data.Pair
import Data.Lambda
import Control.Compose (ToOI(..),Cofunctor(..),Flip(..))

import Interface.TV

{--------------------------------------------------------------------
    TV type specializations
--------------------------------------------------------------------}

type In  = Input  MkI
type Out = Output MkI MkO

type GTV = TV MkI MkO

-- | Type specialization of 'tv'
gtv :: Out a -> a -> GTV a
gtv = tv

-- | Type specialization of 'runTV'
runGTV :: GTV a -> IO ()
runGTV = runTV

runGTVInWidget :: GTV a -> IO Widget
runGTVInWidget tval = unFlip (toOI' (output o)) a
  where (o,a) = unTv tval

-- Equivalently:
-- 
--   runGTV :: RunTV MkI MkO


{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

-- | Add post-processing.  (Could use 'fmap' instead, but 'result' is more
-- specifically typed.)
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)

-- | Add pre-processing.
argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument = flip (.)

infixr 1 ~>
-- | Add pre- and post processing
(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
f ~> h = result h . argument f

-- (f ~> h) g = h . g . f

-- More generally,
-- 
-- (~>) :: Category (-->) => (a' --> a) -> (b --> b') -> ((a --> b) -> (a' --> b'))

-- If I add argument back to DeepArrow, we can get a different generalization:
-- 
-- (~>) :: DeepArrow (-->) => (a' --> a) -> (b --> b') -> ((a -> b) --> (a' -> b'))
-- f ~> h = result h . argument f  -- generalized (.)


{--------------------------------------------------------------------
    Actions & info sinks
--------------------------------------------------------------------}

-- | Convenient shorthand
type Action = IO ()

-- | Sink of information
type Sink a = a -> Action

infixl 1 >+>  -- first guess
-- | Combine sinks
(>+>) :: Sink a -> Sink b -> Sink (a,b)
(snka >+> snkb) (a,b) = snka a >> snkb b


{--------------------------------------------------------------------
    Representations
--------------------------------------------------------------------}

-- Make a input UI.
newtype MkI  a = MkI { unMkI :: MkI' a }

inMkI :: (MkI' a -> MkI' b) -> (MkI a -> MkI b)
inMkI = unMkI ~> MkI

inMkI2 :: (MkI' a -> MkI' b -> MkI' c) -> (MkI a -> MkI b -> MkI c)
inMkI2 = unMkI ~> inMkI

-- Representation type for 'MkI'.  Produces a widget, a polling operation,
-- a termination clean-up action, and a sink for a refresh action to be
-- constructed later.
type MkI' a = IO (Widget, IO a, Action, Sink Action)

-- TODO: Look to reformulate to make the explicit class instances unnecessary.

-- Make an output UI.
newtype MkO a = MkO { unMkO :: MkO' a }

inMkO :: (MkO' a -> MkO' b) -> (MkO a -> MkO b)
inMkO = unMkO ~> MkO

inMkO2 :: (MkO' a -> MkO' b -> MkO' c) -> (MkO a -> MkO b -> MkO c)
inMkO2 = unMkO ~> inMkO


{--------------------------------------------------------------------
    Instances
--------------------------------------------------------------------}

-- Representation type for 'MkO'.  Produce a widget, a way to send it new
-- info to display, and a termination clean-up action.
type MkO' a = IO (Widget, Sink a, Action)

-- Currently, the clean-up actions are created only by clockDtI, and just
-- propagated by the other combinators.

instance Functor MkI where
  fmap f = inMkI (fmap f')
    where
      f' (wid,poll,clean,install) = (wid, fmap f poll, clean, install)

-- Or
-- 
--   fmap = inMkI . fmap . h
--     where
--       h f (wid,poll,clean,install) = (wid, fmap f poll, clean, install)

-- Or
-- 
--   fmap = inMkI . fmap . \ f (wid,poll,clean,install) -> (wid, fmap f poll, clean, install)

-- Better yet: tweak the representation so that Functor is derived.

instance Cofunctor MkO where
  cofmap f = inMkO (fmap f')
   where
     f' (wid,sink,cleanup) = (wid, sink . f, cleanup)

--   cofmap f (MkO mk) = MkO (fmap f' mk)
--    where
--      f' (wid,sink,cleanup) = (wid, sink . f, cleanup)

-- Note that Functor & Cofunctor are isomorphic to a standard form.
-- Consider redefining MkI' and MkO' accordingly.  See how other instances
-- work out.

forget2 :: Monad m => (w -> a -> m b) -> (w -> a -> m ())
forget2 = (result.result) ( >> return ())

-- forget2 h w a = h w a >> return ()

instance CommonIns MkI where
  getString start = MkI $
    do w <- entryNew
       entrySetText w start
       return (toWidget w, entryGetText w, return (), forget2 onEntryActivate w)
  getRead = getReadF  -- thanks to MkI Functor
  getBool start = MkI $
    do w <- checkButtonNew
       toggleButtonSetActive w start
       return (toWidget w, toggleButtonGetActive w, return (), forget2 onToggled w)

instance CommonOuts MkO where
  putString = MkO $
    do entry <- entryNew
       return (toWidget entry, entrySetText entry, return ())
  putShow = putShowC  -- thanks to MkO Cofunctor
  putBool = MkO $
    do w <- checkButtonNew
       return (toWidget w, toggleButtonSetActive w, return ())

-- For temporary use, since OpenGL added a 'set' between 2.2.3.0 and 2.4.0.1
gset :: o -> [AttrOp o] -> IO ()
gset = Gtk.set

boxed :: Orient -> Widget -> Widget -> IO Widget
boxed o wa wb =
  do box <- boxNew o False 10
     gset box [ containerChild := wa , containerChild := wb ]
     return (toWidget box)

hboxed :: Widget -> Widget -> IO Widget
hboxed = boxed Horizontal

instance Pair MkI where
  pair = inMkI2 $ \ ia ib ->
    do (wa,geta,cleana,installa) <- ia
       (wb,getb,cleanb,installb) <- ib
       box <- wa `hboxed` wb
       return ( box
              , liftA2 (,) geta getb
              , cleana >> cleanb
              , liftA2 (>>) installa installb
              )

instance Pair MkO where
  pair = inMkO2 $ \ oa ob ->
    do (wa,snka,cleana) <- oa
       (wb,snkb,cleanb) <- ob
       box <- wa `hboxed` wb
       return (box, snka >+> snkb, cleana >> cleanb)

-- These Pair instances are getting closer to vanishing.  Regular
-- structure is emerging. Keep inching along.  One wart is that hboxed is
-- an IO operation, unlike the other combiners.

instance Title_f MkI where
  title_f str = inMkI $ \ ia ->
    do (widget,geta,cleana,installa) <- ia
       frame  <- frameNew
       gset frame [ frameLabel      := str
                  , containerBorderWidth := 5
                 , containerChild  := widget ]
       return (toWidget frame, geta, cleana, installa)

instance Title_f MkO where
  title_f str = inMkO $ \ oa ->
   do (widget,sink,clean) <- oa
      frame  <- frameNew
      gset frame [ frameLabel      := str
                 -- , frameShadowType := ShadowEtchedOut
                 , containerBorderWidth := 5
                 , containerChild  := widget ]
      return (toWidget frame, sink, clean)

instance Lambda MkI MkO where
  lambda = (unMkI ~> unMkO ~> MkO) $ \ ia ob ->
    do box  <- boxNew Vertical False 10
       reff <- newIORef (error "mkLambda: no function yet")
       (wa,geta,cleana,installa) <- ia
       (wb,snkb,cleanb         ) <- ob
       let refresh = readIORef reff <*> geta >>= snkb
       installa refresh
       -- set box [ containerChild := wa , containerChild := wb ]
       -- Hack: stretch output but not input.  Really I want to choose
       -- per widget and propagate upward.
       boxPackStart box wa PackNatural 0
       boxPackStart box wb PackGrow    0
       return ( toWidget box
              , \ f -> writeIORef reff f >> refresh
              , cleana >> cleanb)


{--------------------------------------------------------------------
    Execution
--------------------------------------------------------------------}

runMkO :: String -> MkO a -> a -> Action
runMkO = (result.result.argument) return runMkOIO

runMkO' :: MkO a -> a -> IO Widget 
runMkO' mko =  runMkOIO' mko . return

-- runMkO name mko = runMkOIO name mko . return

runMkOIO :: String -> MkO a -> IO a -> Action
runMkOIO name (MkO mko') mkA = do
  forget initGUI
  (wid,sink,cleanup) <- mko'
  window <- windowNew
  gset window [ windowDefaultWidth   := 200
           -- , windowDefaultHeight  := 200
              , containerBorderWidth := 10
              , containerChild       := wid
           -- , windowFocusOnMap     := True       -- helpful?
              , windowTitle          := name
              ]
  forget $ onDestroy window (cleanup >> mainQuit)
  widgetShowAll window
  -- Initial sink.  Must come after show-all for the GLDrawingArea.
  mkA >>= sink
  mainGUI
  return ()
  
runMkOIO' :: MkO a -> IO a -> IO Widget
runMkOIO' (MkO mko') mkA = do
  (wid,sink,cleanup) <- mko'
  forget $ onDestroyEvent wid (\_ -> cleanup >> return True)
  mkA >>= sink
  return wid

instance ToOI MkO where
  toOI mkO = Flip (runMkO "GtkTV" mkO)

toOI' :: MkO a -> Flip (->) (IO Widget) a
toOI' mkO = Flip (runMkO' mkO)

-- | Run a visualization on a constructed ('IO'-extracted) value.  The
-- action is executed just once, after the visualization is all set up,
-- which allows for things like OpenGL shader compilation.
runOutIO :: String -> Out a -> IO a -> Action
runOutIO name out = runMkOIO name (output out)

runOut :: String -> Out a -> a -> Action
runOut = (result.result.argument) return runOutIO
-- runOut name out = runOutIO name out . return

-- I'd like to eliminate the glew dependency, and I don't know how.  The
-- ToOI method doesn't get a chance to pass in specialized info.  Hm.


{--------------------------------------------------------------------
    UI primitives
--------------------------------------------------------------------}


data Orient = Horizontal | Vertical deriving (Read,Show)

boxNew :: Orient -> Bool -> Int -> IO Box
boxNew Vertical   = boxer vBoxNew
boxNew Horizontal = boxer hBoxNew

boxer :: BoxClass box => (a -> b -> IO box) -> (a -> b -> IO Box)
boxer = (result.result.fmap) toBox

primMkI :: MkI' a -> In a
primMkI = iPrim . MkI

-- Currently unused

primMkO :: MkO' a -> Out a
primMkO = oPrim . MkO

type R = Float

-- TODO: Consider using R == Double (for constant folding), while really
-- being float on the GLSL side.

sliderRIn :: (R,R) -> R -> In R
sliderRIn = sliderGIn realToFrac realToFrac 0.005 5

sliderIIn :: (Int,Int) -> Int -> In Int
sliderIIn = sliderGIn fromIntegral round 1 0

-- The step argument indicates how big a jump to make when clicking to one
-- side of the slider tab.  Seems to be a fraction of the whole range,
-- rather than a fixed amount.  I don't know what makes a good choice.

-- Generalized slider.  Gtk's scaling widgets work with Double, so this
-- adapter takes some initial params for conversion.  Only fires when a
-- value really changes.
sliderGIn :: (Show a, Eq a) => (a -> Double) -> (Double -> a) -> a -> Int
            -> (a,a) -> a -> In a
sliderGIn toD fromD step digits
             (lo,hi) a0 = primMkI $
  do oldRef <- newIORef a0
     w <- hScaleNewWithRange (toD lo) (toD hi) (toD step)
     gset w [ rangeValue := toD a0, scaleDigits := digits ]
     let getter = fromD <$> get w rangeValue
         install refresh = forget2 afterRangeChangeValue w
                             (\ _ x -> changeTo (fromD x) >> return False)
          where
            changeTo new =
              do old <- readIORef oldRef
                 when (old /= new) $
                      do forget refresh
                         writeIORef oldRef new
     -- TODO: experiment with return False vs True
     return (toWidget w, getter, return (), install)

-- -- Prevent vertical stretching
-- noVert :: WidgetClass w => w -> IO Widget
-- noVert w = do b <- boxNew Vertical False 0
--               boxPackStart b w PackNatural 0
--               return (toWidget b)


-- | A clock that reports time in seconds and updates at the given period
-- (in seconds).
clockDtI :: R -> In R
clockDtI period = primMkI $
  do start <- time
     -- Start with a do-nothing refresh action.
     refreshRef <- newIORef (return ())
     timeout <- timeoutAddFull (join (readIORef refreshRef) >> return True)
                  priorityDefaultIdle (round (period * 1000))
     w <- vBoxNew True 0    -- size 0 box
     return ( toWidget w, subtract start <$> time
            , timeoutRemove timeout, writeIORef refreshRef )


-- Deactivating the clock's timeout during clean-up prevents it from
-- running when gtk starts up again.  Particularly useful in ghci, where
-- restarting gtk is commonplace.

-- | A clock that updates every 1/60 second
clockIn :: In R
clockIn = clockDtI (1/60)

-- Get the time since midnight, in seconds
time :: IO R
time = (fromRational . toRational . utctDayTime) <$> getCurrentTime


-- | Rate slider.  Convenience function built from 'sliderRin' and 'integralDtIn'.
rateSliderDtIn :: R -> (R,R) -> R -> In R
rateSliderDtIn period = (result.result) (integralDtIn period) sliderRIn

-- | Rate slider.  Updates result (integral) 60 times per second.
-- Specialization of 'rateSliderDtIn'.
rateSliderIn :: (R,R) -> R -> In R
rateSliderIn = rateSliderDtIn (1/60)

-- | Integral of an input, with given update interval (in seconds)
integralDtIn :: (VectorSpace v, Eq v, Scalar v ~ Float) =>
                R -> In v -> In v
integralDtIn period inp = primMkI $
  do refT  <- time >>= newIORef
     refX  <- newIORef zeroV
     refreshRef <- newIORef (return ())
     (w,getV,cleanV,_) <- mkI'
     timeout <- timeoutAddFull (join (readIORef refreshRef) >> return True)
                  priorityDefaultIdle (round (period * 1000))
     let getX = do v <- getV
                   prevX <- readIORef refX
                   if (v /= zeroV) then
                     do t <- time
                        prevT <- readIORef refT
                        let x = prevX ^+^ (t - prevT) *^ v
                        writeIORef refT t
                        writeIORef refX x
                        return x
                    else
                       return prevX
     return (w, getX, timeoutRemove timeout >> cleanV, writeIORef refreshRef)
 where
   MkI mkI' = input inp

-- Better: getX changes no state.  Instead, update refT & refX when slider changes.
-- In any case, only invoke refresh when the rate is nonzero

-- | Integral of an input.  Updates result (integral) 60 times per second.
integralIn :: (VectorSpace v, Eq v, Scalar v ~ Float) =>
              In v -> In v
integralIn = integralDtIn (1/60)


fileNameIn :: FilePath -> In FilePath
fileNameIn start = primMkI $
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     forget $ fileChooserSetFilename w start
     return ( toWidget w
            , fromMaybe start <$> fileChooserGetFilename w
            , return ()
            , forget2 onCurrentFolderChanged w
            )


-- TODO: Replace the error message with a GUI version.

-- We're freeing the old thingie before saving the new thingie.  In a
-- multi-threaded setting, there could be dire consequences.

-- I'd like to move to a consistently GC'd setting, in which textures,
-- shaders, etc are GC'd.  In that case, what keeps GPU resources alive?



{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

forget :: Functor f => f a -> f ()
forget = (() <$)
-- forget = fmap (const ())

