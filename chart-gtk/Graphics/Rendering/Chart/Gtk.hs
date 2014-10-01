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

import Graphics.UI.Gtk.Gdk.GC

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.State(EC, execEC)

import Data.IORef
import Data.Default.Class

import Control.Monad(when)
import System.IO.Unsafe(unsafePerformIO)

data PanState = PanState AxisTransform (Maybe Range)

-- | transform that defines the normalized axis transform
type AxisTransform = (Double,Double)

-- | class for the types of things that can be zoomed and rendered
class ToRenderable z => Zoomable z where
  -- | Window Dims -> xtransform -> ytransform -> Zoomable -> Zoomable
  transform :: AxisTransform -> AxisTransform -> z -> z

instance (PlotValue x, PlotValue y) => Zoomable (Layout x y) where
  transform (p1x, p2x) (p1y,p2y) l = newaxis where
    laxisx = transformAxis (p1x,p2x) (_layout_x_axis l)
    laxisy = transformAxis (p1y,p2y) (_layout_y_axis l)
    newaxis = l {_layout_x_axis = laxisx
                , _layout_y_axis = laxisy }

instance (PlotValue x, PlotValue y1, PlotValue y2) => Zoomable (LayoutLR x y1 y2) where
  transform (p1x,p2x) (p1y,p2y) lr = newaxis where
    laxisx = transformAxis (p1x,p2x) (_layoutlr_x_axis lr)
    laxisy1 = transformAxis (p1y,p2y) (_layoutlr_left_axis lr)
    laxisy2 = transformAxis (p1y,p2y) (_layoutlr_right_axis lr)
    newaxis = lr { _layoutlr_x_axis = laxisx
                 , _layoutlr_left_axis = laxisy1
                 , _layoutlr_right_axis = laxisy2 }


-- | Transform the apparent device coordinates based on the normalized
-- view encoded by the AxisTransform pair
transformAxis :: PlotValue x => AxisTransform -> LayoutAxis x -> LayoutAxis x
transformAxis (a,b) axis = let
  override ax = ax' where
    ax' = autoAxis [l,r]
    l = _axis_tropweiv ax (0,1) a
    r = _axis_tropweiv ax (0,1) b
  in axis { _laxis_override = override } -- . (_laxis_override axis) }

------------------------------------------------------------

-- | bits for generating/modifying/querying the state of zoom
data ZoomState =  ZoomState { press :: Maybe Range -- device
                            , mouse_drag :: Maybe Range -- device
                            , panstate :: PanState -- device
                            , xrange :: AxisTransform
                            , yrange :: AxisTransform }

type ZoomStack = [ZoomState]

zoomZero :: ZoomState
zoomZero = ZoomState { press = Nothing
                     , mouse_drag = Nothing
                     , panstate = PanState (0,0) Nothing
                     , xrange = (0,1)
                     , yrange = (0,1) }


zoomTransform :: Zoomable z => ZoomStack -> z -> z
zoomTransform zs z = z' where
  z' = case (getPan zs, getXrange zs, getYrange zs) of
     (PanState (0,0) _, (0,1), (0,1)) -> z
     (PanState (px,py) _,xr, yr) -> z' where
          z' = transform xr' yr' z
          xr' = propagate xr (px,1+px)
          yr' = propagate yr (py,1+py)

-- | Remove a zoom from the stack
popZoom :: ZoomStack -> ZoomStack
popZoom h@[_] = h
popZoom (_:t) = t

getXrange :: ZoomStack -> Range
getXrange = xrange . head

getYrange :: ZoomStack -> Range
getYrange = yrange . head

getPress :: ZoomStack -> Maybe Range
getPress (ZoomState{ press = r }:_) = r

setPress :: Range -> ZoomStack -> ZoomStack
setPress r (z:zs) = (z { press = Just r }):zs

clearPress :: ZoomStack -> ZoomStack
clearPress (z:zs) = (z { press = Nothing }):zs

getDrag :: ZoomStack -> Maybe Range
getDrag (ZoomState{ mouse_drag = d }:_) = d

setDrag :: Range -> ZoomStack -> ZoomStack
setDrag r (z:zs) = (z { mouse_drag = Just r }):zs

clearDrag :: ZoomStack -> ZoomStack
clearDrag (z:zs) = (z { mouse_drag = Nothing }):zs

getPan :: ZoomStack -> PanState
getPan (ZoomState{panstate = p }:_) = p

setPan :: PanState -> ZoomStack -> ZoomStack
setPan p (h:hs) = h':hs where
  h' = h { panstate = p }

setPanPress :: Maybe Range -> ZoomStack -> ZoomStack
setPanPress r zs = setPan p zs where
  p = PanState a r
  PanState a _ = getPan zs

-----------------------------------------------------------

-- | Some constants TODO: these may not actually be 'constant'
leftmargin, rightmargin, topmargin, bottommargin :: Double
leftmargin   = 39
rightmargin  = 16
topmargin    = 38
bottommargin = 54


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

toZoomableWindow :: (Default z, Zoomable z) => Int -> Int -> EC z () -> IO ()
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

createZoomableWindow :: Zoomable z => z -> Int -> Int -> IO G.Window
createZoomableWindow z windowWidth windowHeight = do
    zooms <- newIORef [zoomZero]
    window <- G.windowNew
    vbox <- G.vBoxNew False 0
    canvas <- G.drawingAreaNew
    menu <- makeMenu window canvas zooms z
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.boxPackStart vbox menu G.PackNatural 0
    G.boxPackEnd vbox canvas G.PackGrow 0
    G.set window [G.containerChild G.:= vbox]
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.onExpose canvas $ const $ do
      zs <- readIORef zooms
      let r = toRenderable $ zoomTransform zs z
      _updateCanvas False r canvas
      drawMouseBox zs canvas
      G.widgetGetDrawWindow canvas >>= G.drawWindowEndPaint -- manually finish canvas
      return True

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.onMotionNotify canvas True (mouseMotion zooms canvas)
    return window

-- | Draws the mouse selection box
drawMouseBox :: ZoomStack -> G.DrawingArea -> IO Bool
drawMouseBox zoom canvas = do
  case (getDrag zoom, getPress zoom) of
    (Just (x,y), Just (x',y')) -> do
      let x'' = round $ min x x'
          y'' = round $ min y y'
          width = round $ abs (x - x')
          height = round $ abs (y - y')

      if x'' >= round leftmargin && y'' >= round rightmargin
        then do
          win <- G.widgetGetDrawWindow canvas
          gc <- gcNew win
          G.drawRectangle win gc False x'' y'' width height
          return True
        else return True

    -- All other cases we dont draw
    _ -> return True

-- | Saves the mouse position to draw the selection rectangle
mouseMotion :: IORef ZoomStack -> G.DrawingArea -> GE.Event -> IO Bool
mouseMotion zooms canvas GE.Motion { GE.eventX = x, GE.eventY = y} = do
  zs <- readIORef zooms
  case (getPress zs, getPan zs) of
    (Just _, _) -> do
      (w,h) <- dwidth canvas
      writeIORef zooms $ setDrag (restrictX w x, restrictY h y) zs
      G.widgetQueueDraw canvas
      return True

    (_, PanState (dx,dy) (Just (lx, ly))) -> do
      (w,h) <- dwidth canvas
      let dx' = dx-(x-lx)/w
          dy' = dy+(y-ly)/h -- the axis device coords are flipped
      writeIORef zooms $ setPan (PanState (dx',dy') (Just (x,y))) zs
      G.widgetQueueDraw canvas
      return True

    _ -> return True

makeMenu :: Zoomable z => G.Window -> G.DrawingArea -> IORef ZoomStack -> z ->  IO G.MenuBar
makeMenu window canvas ref z = do
  menuBar <- G.menuBarNew
  filemenu <- G.menuItemNewWithMnemonic "_File"
  G.set menuBar [G.containerChild G.:= filemenu]
  menu <- G.menuNew
  filemenu `G.menuItemSetSubmenu` menu
  save <- G.menuItemNewWithMnemonic "_Save"
  save `G.on` G.menuItemActivate $ do
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
            zs <- readIORef ref
            renderableToFile def p $ toRenderable $ zoomTransform zs z
            return ()
      _ -> return () -- shouldn't get here.

    G.widgetDestroy chooser

  quit <- G.menuItemNewWithMnemonic "_Quit"
  G.on quit G.menuItemActivate G.mainQuit
  G.set menu [G.containerChild G.:= save, G.containerChild G.:= quit]

  return menuBar


-- Handles the button events
onButtonEvent :: IORef ZoomStack -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dwidth canvas
  modifyIORef ref $ \z -> setPress (restrictX w (GE.eventX e),restrictY h (GE.eventY e)) z

  G.widgetQueueDraw canvas
  return True

onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.ReleaseClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dwidth canvas
  r <- onButtonRelease ref canvas (restrictX w (GE.eventX e), restrictY h (GE.eventY e))
  G.widgetQueueDraw canvas
  return r

onButtonEvent ref canvas (GE.Button { GE.eventClick = GE.SingleClick,
                                      GE.eventButton = GE.RightButton }) = do
  modifyIORef ref popZoom
  G.widgetQueueDraw canvas
  return True

-- | Pan click
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.MiddleButton }) = do
  (w,h) <- dwidth canvas
  let x = restrictX w (GE.eventX e)
      y = restrictY h (GE.eventY e)
  modifyIORef ref $ setPanPress (Just (x,y))
  return True

-- | Pan release
onButtonEvent ref _ (GE.Button{ GE.eventClick = GE.ReleaseClick,
                                       GE.eventButton = GE.MiddleButton }) = do
  modifyIORef ref $ setPanPress Nothing
  return True

-- | Other mouse events
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

-- | Propegates the scale if the system has already been scaled
propagate :: AxisTransform -> AxisTransform -> AxisTransform
propagate (a,b) (a',b') = (a'',b'') where
  a'' = a + a'*w
  b'' = a + b'*w
  w = b - a

onButtonRelease :: IORef ZoomStack
                    -> G.DrawingArea
                    -> Range
                    -> IO Bool
onButtonRelease ref canvas (x,y) = let
    go (width,height) (z@ZoomState{ press = Just (x',y')
                                  , panstate = PanState (px,py) _
                                  , xrange = xr
                                  , yrange = yr }:zs) = zs' where

        zs' = if dx < 1.0 || dy < 1.0 then clearzoom:zs else z':clearzoom:zs
        z' = zoomZero { xrange = propagate xr (p1x,p2x)
                      , yrange = propagate yr (p1y,p2y) }
        clearzoom = z { press = Nothing, mouse_drag = Nothing }
        dx = abs (x - x')
        dy = abs (y - y')
        width' = width-leftmargin-rightmargin
        height' = height-bottommargin-topmargin
        p1x = (min x x' - leftmargin)/width' + px
        p2x = (max x x' - leftmargin)/width' + px
        p2y = 1 - (min y y' - topmargin)/height' + py
        p1y = 1 - (max y y' - topmargin)/height' + py

  in do
    wh <- dwidth canvas
    modifyIORef ref $ go wh
    return True

updateCanvas :: Renderable a -> G.DrawingArea -> IO Bool
updateCanvas = _updateCanvas True

_updateCanvas :: Bool -> Renderable a -> G.DrawingArea -> IO Bool
_updateCanvas finish chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    regio <- G.regionRectangle $ GE.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    G.drawWindowBeginPaintRegion win regio
    G.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
    if finish
      then G.drawWindowEndPaint win >> return True
      else return True

