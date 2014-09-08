-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Gtk(
    renderableToWindow,
    toWindow,
    createRenderableWindow,
    updateCanvas
    ) where

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.State(EC, execEC)

import Data.List (isPrefixOf)
import Data.IORef
import Data.Default.Class

import Control.Monad(when)
import System.IO.Unsafe(unsafePerformIO)


-- do action m for any keypress (except modified keys)
anyKey :: (Monad m) => m a -> GE.Event -> m Bool
anyKey m (GE.Key {GE.eventModifier=[]}) = m >> return True
anyKey _ _ = return True

-- Yuck. But we really want the convenience function
-- renderableToWindow as to be callable without requiring
-- initGUI to be called first. But newer versions of
-- gtk insist that initGUI is only called once
guiInitVar :: IORef Bool
{-# NOINLINE guiInitVar #-}
guiInitVar = unsafePerformIO (newIORef False)

initGuiOnce :: IO ()
initGuiOnce = do
    v <- readIORef guiInitVar
    when (not v) $ do
        -- G.initGUI
        G.unsafeInitGUIForThreadedRTS
        writeIORef guiInitVar True

-- | Display a renderable in a gtk window.
--
-- Note that this is a convenience function that initialises GTK on
-- it's first call, but not subsequent calls. Hence it's
-- unlikely to be compatible with other code using gtk. In
-- that case use createRenderableWindow.
renderableToWindow :: Renderable a -> Int -> Int -> IO ()
renderableToWindow chart windowWidth windowHeight = do
    initGuiOnce
    window <- createRenderableWindow chart windowWidth windowHeight
    -- press any key to exit the loop
    G.onKeyPress window $ anyKey (G.widgetDestroy window)
    G.onDestroy window G.mainQuit
    G.widgetShowAll window
    G.mainGUI

-- | Generate a new GTK window from the state content of
-- an EC computation. The state may have any type that is
-- an instance of `ToRenderable`
toWindow :: (Default r, ToRenderable r) =>Int -> Int -> EC r () -> IO ()
toWindow windowWidth windowHeight ec = renderableToWindow r windowWidth windowHeight where
                       r = toRenderable (execEC ec)

-- | Create a new GTK window displaying a renderable.
createRenderableWindow :: Renderable a -> Int -> Int -> IO G.Window
createRenderableWindow chart windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    zooms <- newIORef initialZoom
    G.widgetSetSizeRequest window windowWidth windowHeight
    G.onExpose canvas $ const (updateCanvas chart canvas)
    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.set window [G.containerChild G.:= canvas]
    return window

data Transform = Transform Double Double Double Double
                 deriving (Show)

data ZoomState = ZoomState { press :: Maybe (Double,Double),
                             stack :: [Transform] } deriving (Show)
initialZoom = ZoomState { press = Nothing, stack = [] }

onButtonEvent :: IORef ZoomState -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref _  (e@GE.Button{ GE.eventClick = GE.SingleClick,
                                   GE.eventButton = GE.LeftButton }) = do
  putStrLn $ show e
  onButtonPress ref (GE.eventX e) (GE.eventY e)

onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.ReleaseClick,
                                  GE.eventButton = GE.LeftButton }) = do
  putStrLn $ show e
  onButtonRelease ref canvas (GE.eventX e) (GE.eventY e)

onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.SingleClick,
                                       GE.eventButton = GE.RightButton }) = do
  putStrLn $ show e
  popStack ref
  state <- readIORef ref
  putStrLn $ show state
  return True

onButtonEvent _ _ _ = return False

onButtonPress ref x y = do
  modifyIORef ref $ \z -> z{ press = Just (x,y) }
  return True

popStack :: IORef ZoomState -> IO ()
popStack ref = modifyIORef ref go where
  go (z@ZoomState { stack=x:xs }) = z { stack = xs }
  go z = z

onButtonRelease :: IORef ZoomState -> G.DrawingArea -> Double -> Double -> IO Bool
onButtonRelease ref canvas x y = let
    go (z@ZoomState{ press = Just (x',y'), stack =stack }) =
      z { press=Nothing, stack = stack' } where
        stack' = if dx < 1.0 && dy < 1.0 then stack
                 else t:stack
                   where
                     dx = abs (x-x')
                     dy = abs (y-y')
                     t = Transform (min x x') dx (min y y') dy

    go z = z -- Don't know what this is
  in do {modifyIORef ref go; return True }

updateCanvas :: Renderable a -> G.DrawingArea  -> IO Bool
updateCanvas chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    regio <- G.regionRectangle $ GE.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    G.drawWindowBeginPaintRegion win regio
    G.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
    G.drawWindowEndPaint win
    return True
