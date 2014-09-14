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
import qualified Graphics.UI.Gtk.Selectors.FileChooserDialog as FC
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


-- | types associated with the zoom functionality
type Transform = (Double, Double, Double, Double)
data ZoomState x y = ZoomState { press :: Maybe (Double,Double),
                             stack :: [Layout x y]}

-- | Some constants
leftmargin = 40.0
rightmargin = 12.0
topmargin = 40.0
bottommargin = 30.0

buttonwidth = 50
buttonheight = 30

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

toZoomableWindow :: forall x y . (Ord x, PlotValue x, Ord y, PlotValue y)
                        => Int
                        -> Int
                        -> EC (Layout x y) ()
                        -> IO ()
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

createZoomableWindow :: forall x y . (Ord x, Ord y, PlotValue x, PlotValue y)
                                 => Layout x y
                                 -> Int
                                 -> Int
                                 -> IO G.Window
createZoomableWindow layout windowWidth windowHeight = do
    zooms <- newIORef $ initialZoom layout
    window <- G.windowNew
    vbox <- G.vBoxNew False 2
    G.widgetSetSizeRequest vbox (buttonwidth+windowWidth) (buttonheight+windowHeight)
    canvas <- G.drawingAreaNew
    hbox <- makeMenu window zooms
    G.set vbox [G.containerChild G.:= hbox, G.containerChild G.:= canvas]
    G.set window [G.containerChild G.:= vbox]
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.onExpose canvas $ const $ do
      zoom <- readIORef zooms
      let l = case stack zoom of
            (t:_) -> t
            []    -> layout
      updateCanvas (toRenderable l) canvas

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    return window

makeMenu :: (PlotValue x, PlotValue y)
         => G.Window
         -> IORef (ZoomState x y)
         -> IO G.HBox
makeMenu window ref = do
  hbox <- G.hBoxNew False 0
  G.widgetSetSizeRequest hbox 0 buttonheight
  align <- G.alignmentNew 0 0 0 1
  saveButton <- G.buttonNewWithMnemonic "_Save"
  G.widgetSetSizeRequest saveButton buttonwidth buttonheight
  G.set align [G.containerChild G.:= saveButton]
  G.set hbox [G.containerChild G.:= align]
  G.onButtonPress saveButton $ const $ do
    putStrLn "Save pressed!"
    chooser <- FC.fileChooserDialogNew Nothing (Just window)
                          G.FileChooserActionSave
                          [("Save",G.ResponseAccept),("Cancel",G.ResponseCancel)]
    G.fileChooserSetDoOverwriteConfirmation chooser True
    result <- G.dialogRun chooser
    case result of
      G.ResponseCancel -> return ()
      G.ResponseAccept -> do
        m <- G.fileChooserGetFilename chooser
        case m of
          Nothing -> return ()
          Just p  -> do
            ZoomState{ stack = t:_} <- readIORef ref
            renderableToFile def p (toRenderable t)
            return ()

    G.widgetDestroy chooser
    return True

  return hbox

-- | Transform the axis of the layout to cause the 'zoom' type effect
transformLayout :: (PlotValue x, PlotValue y) => Transform -> Layout x y -> Layout x y
transformLayout (p1x, p1y, p2x, p2y) l = let
    transformaxis p1 p2 ad =
      let
        oldviewport = _axis_viewport ad
        oldtropweiv = _axis_tropweiv ad

        newviewport r x = vmap (xmin,xmax) r x where
          xmin = oldtropweiv r p1
          xmax = oldtropweiv r p2

        newtropweiv r d = invmap (xmin,xmax) r d where
          xmin = oldtropweiv r p1
          xmax = oldtropweiv r p2

      in ad { _axis_viewport = newviewport
            , _axis_tropweiv = newtropweiv }

    oldxaxisfn = _laxis_override $ _layout_x_axis l
    oldyaxisfn = _laxis_override $ _layout_y_axis l

    laxisx = (_layout_x_axis l) {_laxis_override = transformaxis p1x p2x . oldxaxisfn }
    laxisy = (_layout_y_axis l) {_laxis_override = transformaxis p1y p2y . oldyaxisfn }
    newaxis = l{_layout_x_axis = laxisx, _layout_y_axis = laxisy}

    in newaxis

initialZoom l = ZoomState { press = Nothing, stack = [l] }

-- Handles the button events
onButtonEvent :: (PlotValue x, PlotValue y) => IORef (ZoomState x y) -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.SingleClick,
                                   GE.eventButton = GE.LeftButton }) = do
  -- putStrLn $ show e
  (w,h) <- dwidth canvas
  modifyIORef ref $ \z -> z{ press = Just (restrictX w (GE.eventX e),restrictY h (GE.eventY e)) }
  G.widgetQueueDraw canvas
  return True

onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.ReleaseClick,
                                  GE.eventButton = GE.LeftButton }) = do
  state <- readIORef ref
  r <- onButtonRelease ref canvas (GE.eventX e) (GE.eventY e)
  G.widgetQueueDraw canvas
  return r

onButtonEvent ref canvas (e@GE.Button{ GE.eventClick = GE.SingleClick,
                                       GE.eventButton = GE.RightButton }) = do
  state <- readIORef ref
  case stack state of
    [_]   -> return ()
    (h:t) -> writeIORef ref (state { stack = t })

  G.widgetQueueDraw canvas
  return True

onButtonEvent _ _ _ = return False
-- End of button events


-- | restricts the point on the canvas to the plot area
restrictX width x | x < leftmargin          = leftmargin
                  | x > width - rightmargin = width - rightmargin
                  | otherwise               = x

restrictY height y | y < topmargin             = topmargin
                   | y > height - bottommargin = height - bottommargin
                   | otherwise                 = y

-- | get the width of the canvas as a pair of Double's
dwidth :: G.DrawingArea -> IO (Double, Double)
dwidth canvas = do
  (w, h) <- G.widgetGetSize canvas
  return (fromIntegral w, fromIntegral h)


onButtonRelease :: (PlotValue x, PlotValue y) => IORef (ZoomState x y)
                                             -> G.DrawingArea
                                             -> Double
                                             -> Double
                                             -> IO Bool
onButtonRelease ref canvas x y = let
    go width height (z@ZoomState{ press = Just (x',y'), stack = stack@(t:_) }) =
      z { press=Nothing, stack = stack' } where
        stack' = if dx < 1.0 && dy < 1.0 then stack else t':stack where
                   x'' = restrictX width x
                   y'' = restrictY height y
                   dx = abs (x' - x'')
                   dy = abs (y' - y'')
                   p1x = (min x' x'') - leftmargin
                   p2x = (max x' x'') - leftmargin
                   p1y = (max y' y'') - topmargin
                   p2y = (min y' y'') - topmargin

                   t' = transformLayout (p1x, p1y, p2x, p2y) t

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
