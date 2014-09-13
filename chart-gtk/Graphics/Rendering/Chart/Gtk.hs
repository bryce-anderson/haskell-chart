-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Rendering.Chart.Gtk(
    renderableToWindow,
    toWindow,
    toZoomableWindow,
    createRenderableWindow,
    createZoomableWindow,
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

import Debug.Trace


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
renderableToWindow chart windowWidth windowHeight = makeWindow window where
    window = createRenderableWindow chart windowWidth windowHeight

makeWindow :: IO G.Window -> IO ()
makeWindow w = do
    initGuiOnce
    window <- w
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

toZoomableWindow :: forall x y . (Ord x, PlotValue x, Ord y, PlotValue y) => Int -> Int -> EC (Layout x y) () -> IO ()
toZoomableWindow windowWidth windowHeight ec = makeWindow window where
  window = createZoomableWindow (execEC ec) windowWidth windowHeight

-- | Create a new GTK window displaying a renderable.
createRenderableWindow :: Renderable a -> Int -> Int -> IO G.Window
createRenderableWindow chart windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    G.widgetSetSizeRequest window windowWidth windowHeight
    G.onExpose canvas $ const (updateCanvas chart canvas)
    G.set window [G.containerChild G.:= canvas]
    return window

leftmargin = 40.0
rightmargin = 12.0
topmargin = 40.0
bottommargin = 30.0

createZoomableWindow :: forall x y . (Ord x, Ord y) => Layout x y -> Int -> Int -> IO G.Window
createZoomableWindow layout windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    zooms <- newIORef initialZoom
    G.widgetSetSizeRequest window windowWidth windowHeight
    G.onExpose canvas $ const $ do
      zoom <- readIORef zooms
      (width,height) <- G.widgetGetSize canvas
      let dwidth = fromIntegral width
          dheight = fromIntegral height
          newlayout = case stack zoom of
            (t:_) -> transformLayout dwidth dheight t layout
            []    -> layout
      updateCanvas (toRenderable newlayout) canvas

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.set window [G.containerChild G.:= canvas]
    return window

-- Transform the axis of the layout to cause the 'zoom' type effect
transformLayout :: Double -> Double -> Transform -> Layout x y -> Layout x y
transformLayout windowwidth windowheight (tx, sx, ty, sy) l = let
    transformaxis :: Double -> Double -> Double -> Double -> AxisData x -> AxisData x
    transformaxis w offset t s ad =
      let newviewport r x = (_axis_viewport ad r x - t * w + offset) * s
      in ad { _axis_viewport = newviewport }

    oldxaxisfn = _laxis_override $ _layout_x_axis l
    oldyaxisfn = _laxis_override $ _layout_y_axis l

    laxisx = (_layout_x_axis l) {_laxis_override = transformaxis windowwidth leftmargin tx sx . oldxaxisfn }
    laxisy = (_layout_y_axis l) {_laxis_override = transformaxis windowheight topmargin ty sy . oldyaxisfn }
    newaxis = l{_layout_x_axis = laxisx, _layout_y_axis = laxisy}

    in newaxis

type Transform = (Double, Double, Double, Double)
data ZoomState = ZoomState { press :: Maybe (Double,Double),
                             stack :: [Transform] } deriving (Show)
initialZoom = ZoomState { press = Nothing, stack = [] }

onButtonEvent :: IORef ZoomState -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.SingleClick,
                                   GE.eventButton = GE.LeftButton }) = do
  -- putStrLn $ show e
  (w,h) <- dwidth canvas
  modifyIORef ref $ \z -> z{ press = Just (restrictX w (GE.eventX e),restrictY h (GE.eventY e)) }
  G.widgetQueueDraw canvas
  return True

onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.ReleaseClick,
                                  GE.eventButton = GE.LeftButton }) = do
  -- putStrLn $ show e
  state <- readIORef ref
  -- putStrLn $ show state
  r <- onButtonRelease ref canvas (GE.eventX e) (GE.eventY e)
  G.widgetQueueDraw canvas
  return r

onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.SingleClick,
                                       GE.eventButton = GE.RightButton }) = do
  -- putStrLn $ show e
  popStack ref
  state <- readIORef ref
  -- putStrLn $ show state
  G.widgetQueueDraw canvas
  return True

onButtonEvent _ _ _ = return False

onButtonPress ref width height x y = do
  return True

popStack :: IORef ZoomState -> IO ()
popStack ref = modifyIORef ref go where
  go (z@ZoomState { stack=x:xs }) = z { stack = xs }
  go z = z

-- | restricts the point on the canvas to the plot area
restrictX width x | x < leftmargin          = leftmargin
                  | x > width - rightmargin = width - rightmargin
                  | otherwise               = x

restrictY height y | y < topmargin             = topmargin
                   | y > height - bottommargin = height - bottommargin
                   | otherwise                 = y


dwidth :: G.DrawingArea -> IO (Double, Double)
dwidth canvas = do
  (w, h) <- G.widgetGetSize canvas
  return (fromIntegral w, fromIntegral h)


onButtonRelease :: IORef ZoomState -> G.DrawingArea -> Double -> Double -> IO Bool
onButtonRelease ref canvas x y = let
    go width height (z@ZoomState{ press = Just (x',y'), stack =stack }) =
      z { press=Nothing, stack = stack' } where
        stack' = if dx < 1.0 && dy < 1.0 then stack else t:stack where
                   x'' = restrictX width x
                   y'' = restrictY height y
                   dx = abs (x' - x'')
                   dy = abs (y' - y'')
                   tx = (min x' x'')/width
                   ty = (min y' y'')/height
                   sx = (width - leftmargin - rightmargin)/dx
                   sy = (height - topmargin - bottommargin)/dy

                   (tx', sx', ty', sy') = case stack of
                     (t:_) -> t
                     _     -> (0, 1, 0, 1)

                   t = (tx/sx'+tx', sx*sx', ty/sy'+ty', sy*sy')

    go _ _ _ = initialZoom -- Don't know what this is so we revert to the default
  in do
    (dw, dh) <- dwidth canvas
    modifyIORef ref $ go dw dh
    return True

updateCanvas :: Renderable a -> G.DrawingArea -> IO Bool
updateCanvas chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    regio <- G.regionRectangle $ GE.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    G.drawWindowBeginPaintRegion win regio
    G.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
    G.drawWindowEndPaint win
    return True
