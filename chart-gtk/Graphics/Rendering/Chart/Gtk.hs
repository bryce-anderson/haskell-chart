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

import Data.List (isPrefixOf)
import Data.IORef
import Data.Default.Class

import Control.Monad(when)
import System.IO.Unsafe(unsafePerformIO)


data PanState = PanState Double Double (Maybe (Double, Double))

-- | types associated with the zoom functionality
type Transform = (Double, Double, Double, Double)

-- | transform that defines the normalized axis transform
type AxisTransform = (Double,Double)

transformAxis :: PlotValue x => AxisTransform -> LayoutAxis x -> LayoutAxis x
transformAxis (nl,nr) axis = let

    rml = nr - nl
    invrml = 1/rml

    transforward :: Range -> Range
    transforward (dl,dr) = (dl',dr') where
      w = dr - dl
      w' = w*invrml
      dl' = dl-w*nl
      dr' = dl' + w'

    transformback :: Range -> Range
    transformback (dl',dr') = (dl,dr) where
      w' = dr' - dl'
      w = w' * rml
      dl = dl' + w'*nl
      dr = dl + w

    override :: AxisData x -> AxisData x
    override ax = let
        new_viewport r x = _axis_viewport ax (transforward r) x
        new_tropweiv r d = _axis_tropweiv ax (transformback r) d
      in ax { _axis_viewport = new_viewport
          , _axis_tropweiv = new_tropweiv }

  in axis { _laxis_override = override . (_laxis_override axis) }

data ZoomState z = Zoomable z => ZoomState { press :: Maybe (Double,Double)
                                          , mouse_drag :: Maybe (Double, Double)
                                          , panstate :: PanState
                                          , stack :: [z] }

-- | class for the types of things that can be zoomed and rendered
class ToRenderable z => Zoomable z where
  transform :: Transform -> z -> z

-- | transforms a LayoutAxis using the supplied device coordinates
transformLayoutAxis :: PlotValue x => Range -> LayoutAxis x -> LayoutAxis x
transformLayoutAxis (p1,p2) layoutaxis = let
  transformaxis axisdata = let
        oldtropweiv = _axis_tropweiv axisdata

        newviewport r x = vmap (xmin,xmax) r x where
          xmin = oldtropweiv r p1
          xmax = oldtropweiv r p2

        newtropweiv r d = invmap (xmin,xmax) r d where
          xmin = oldtropweiv r p1
          xmax = oldtropweiv r p2

        (ticks,labels) = tandl where
          pts = map (\(x,_) -> x) $ _axis_ticks axisdata
          axis' = _laxis_generate layoutaxis pts
          tandl = (_axis_ticks axis', _axis_labels axis')


      in axisdata { _axis_viewport = newviewport
                  , _axis_tropweiv = newtropweiv }
                  --, _axis_ticks    = ticks
                  --, _axis_labels   = labels }

  oldaxisfn = _laxis_override layoutaxis
  in layoutaxis { _laxis_override = transformaxis . oldaxisfn }

instance (PlotValue x, PlotValue y) => Zoomable (Layout x y) where
  transform (p1x, p1y, p2x, p2y) l = newaxis where
    laxisx = transformLayoutAxis (p1x,p2x) (_layout_x_axis l)
    laxisy = transformLayoutAxis (p1y,p2y) (_layout_y_axis l)
    newaxis = l {_layout_x_axis = laxisx, _layout_y_axis = laxisy}

instance (PlotValue x, PlotValue y1, PlotValue y2) => Zoomable (LayoutLR x y1 y2) where
  transform (p1x, p1y, p2x, p2y) lr = newaxis where
    laxisx = transformLayoutAxis (p1x,p2x) (_layoutlr_x_axis lr)
    laxisy1 = transformLayoutAxis (p1y,p2y) (_layoutlr_left_axis lr)
    laxisy2 = transformLayoutAxis (p1y,p2y) (_layoutlr_right_axis lr)
    newaxis = lr { _layoutlr_x_axis = laxisx
                 , _layoutlr_left_axis = laxisy1
                 , _layoutlr_right_axis = laxisy2 }



-- | Some constants
leftmargin, rightmargin, topmargin, bottommargin :: Double
leftmargin = 40.0
rightmargin = 12.0
topmargin = 40.0
bottommargin = 30.0

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
createZoomableWindow layout windowWidth windowHeight = do
    zooms <- newIORef $ initialZoom layout
    window <- G.windowNew
    vbox <- G.vBoxNew False 0
    menu <- makeMenu window zooms
    canvas <- G.drawingAreaNew
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.boxPackStart vbox menu G.PackNatural 0
    G.boxPackEnd vbox canvas G.PackGrow 0
    G.set window [G.containerChild G.:= vbox]
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.onExpose canvas $ const $ do
      zoom <- readIORef zooms
      let l = case stack zoom of
            (t:_) -> t
            []    -> layout
      _updateCanvas False (toRenderable l) canvas
      drawMouseBox zoom canvas
      G.widgetGetDrawWindow canvas >>= G.drawWindowEndPaint -- manually finish canvas
      return True

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.onMotionNotify canvas True (mouseMotion zooms canvas)
    return window

-- | Draws the mouse selection box
drawMouseBox :: ZoomState z -> G.DrawingArea -> IO Bool
drawMouseBox zoom canvas = do
  case (mouse_drag zoom, press zoom) of
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
mouseMotion :: IORef (ZoomState z) -> G.DrawingArea -> GE.Event -> IO Bool
mouseMotion zooms canvas GE.Motion { GE.eventX = x, GE.eventY = y} = do
  zoom <- readIORef zooms
  case (press zoom, panstate zoom) of
    (Just _, _) -> do
      writeIORef zooms zoom { mouse_drag = Just (x, y) }
      G.widgetQueueDraw canvas
      return True

    (_, PanState dx dy (Just (lx, ly))) -> do
      writeIORef zooms zoom { panstate = PanState (dx+x-lx) (dy+y-ly) (Just (x,y)) }
      G.widgetQueueDraw canvas
      return True

    _ -> return True

makeMenu :: G.Window -> IORef (ZoomState z) -> IO G.MenuBar
makeMenu window ref = do
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
            ZoomState { stack = t:_} <- readIORef ref
            renderableToFile def p (toRenderable t)
            return ()
      _ -> return () -- shouldn't get here.

    G.widgetDestroy chooser

  quit <- G.menuItemNewWithMnemonic "_Quit"
  G.on quit G.menuItemActivate G.mainQuit
  G.set menu [G.containerChild G.:= save, G.containerChild G.:= quit]

  return menuBar

initialZoom l = ZoomState { press = Nothing
                          , mouse_drag = Nothing
                          , panstate = PanState 0 0 Nothing
                          , stack = [l] }

-- Handles the button events
onButtonEvent :: IORef (ZoomState z) -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dwidth canvas
  modifyIORef ref $ \z -> z { press = Just (restrictX w (GE.eventX e),restrictY h (GE.eventY e)) }
  G.widgetQueueDraw canvas
  return True

onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.ReleaseClick,
                                        GE.eventButton = GE.LeftButton }) = do
  r <- onButtonRelease ref canvas (GE.eventX e) (GE.eventY e)
  G.widgetQueueDraw canvas
  return r

onButtonEvent ref canvas (GE.Button { GE.eventClick = GE.SingleClick,
                                      GE.eventButton = GE.RightButton }) = do
  state <- readIORef ref
  case stack state of
    [_]   -> return ()
    (_:t) -> writeIORef ref (state { stack = t })

  G.widgetQueueDraw canvas
  return True

-- | Pan click
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.MiddleButton }) = do
  (w,h) <- dwidth canvas
  let x = restrictX w (GE.eventX e)
      y = restrictY h (GE.eventY e)
  modifyIORef ref $ \z -> case panstate z of
   PanState dx dy _ -> z { panstate = PanState dx dy (Just (x,y)) }
  return True

-- | Pan release
onButtonEvent ref _ (GE.Button{ GE.eventClick = GE.ReleaseClick,
                                       GE.eventButton = GE.MiddleButton }) = do
  modifyIORef ref $ \z -> case panstate z of
   PanState dx dy _ -> z { panstate = PanState dx dy Nothing }
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


onButtonRelease :: IORef (ZoomState z)
                    -> G.DrawingArea
                    -> Double
                    -> Double
                    -> IO Bool
onButtonRelease ref canvas x y = let
    go (width,height) (z@ZoomState{ press = Just (x',y'), stack = stack@(t:_) }) =
      z { press=Nothing, mouse_drag=Nothing, stack = stack' } where
        stack' = if dx < 1.0 && dy < 1.0 then stack else t':stack where
                   x'' = restrictX width x
                   y'' = restrictY height y
                   dx = abs (x' - x'')
                   dy = abs (y' - y'')
                   p1x = (min x' x'') - leftmargin
                   p2x = (max x' x'') - leftmargin
                   p1y = (max y' y'') - topmargin
                   p2y = (min y' y'') - topmargin

                   t' = transform (p1x, p1y, p2x, p2y) t

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
