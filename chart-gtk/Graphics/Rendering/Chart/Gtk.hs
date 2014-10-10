-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

{-# LANGUAGE ExistentialQuantification,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances   #-}

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

-- | transform that defines the normalized axis transform
type AxisTransform = (Double,Double)

class ToRenderable t => RenderablePlus t a | t -> a  where
  -- | Renders the window and provides a way to scale the chart
  buildRenderable :: t -> Renderable a
  transformt :: (Point,Point) -> Range -> PickFn a -> t -> Maybe t
  dragTransform :: (Point,Point) -> Range -> PickFn a -> t -> Maybe t

instance (PlotValue x, PlotValue y) => RenderablePlus (Layout x y) (LayoutPick x y y) where
  buildRenderable = layoutToRenderable
  dragTransform (Point x y,Point x' y') (width,height) _ l = Just l'
    where
      l' = l { _layout_x_axis = xaxis, _layout_y_axis = yaxis }
      dx = (x - x')/width
      dy = (y' - y)/height
      tw = (dx, 1+dx)
      th = (dy, 1+dy)
      xaxis = transformAxis tw (_layout_x_axis l)
      yaxis = transformAxis th (_layout_y_axis l)


  transformt (p1,p2) _ f l = do
   lp1 <- f p1
   lp2 <- f p2
   case (lp1, lp2) of
     (LayoutPick_PlotArea x1 y1 _, LayoutPick_PlotArea x2 y2 _) -> Just l'
       where
         l' = l { _layout_x_axis = xaxis, _layout_y_axis = yaxis }
         lr = (min x1 x2, max x1 x2)
         tb = (min y1 y2, max y1 y2)
         xoverride' = _laxis_override $ _layout_x_axis l
         yoverride' = _laxis_override $ _layout_y_axis l
         xaxis = (_layout_x_axis l) { _laxis_override = override lr . xoverride' }
         yaxis = (_layout_y_axis l) { _laxis_override = override tb . yoverride' }
         override range ax = _axis_ranged ax range

     _ -> Nothing

{-
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
-}

-- | Transform the apparent device coordinates based on the normalized
-- view encoded by the AxisTransform pair
transformAxis :: PlotValue x => AxisTransform -> LayoutAxis x -> LayoutAxis x
transformAxis (a,b) axis = let
  override ax = ax' where
    ax' = _axis_ranged ax (l,r)
    l = _axis_tropweiv ax (0,1) a
    r = _axis_tropweiv ax (0,1) b
  override' = _laxis_override axis
  in axis { _laxis_override = override . override' } -- . (_laxis_override axis) }

------------------------------------------------------------

-- | bits for generating/modifying/querying the state of zoom
data ZoomState t =  ZoomState { mouse_pos :: Maybe Point
                              , left_press :: Maybe Point
                              , press_complete :: Maybe (Point,Point)
                              , drag_last :: Maybe Point
                              , ts :: [t] }

-- | Remove a zoom from the stack
popZoom :: ZoomState a -> ZoomState a
popZoom h@(ZoomState { ts = [_] }) = h
popZoom h@(ZoomState { ts = (t:ts') }) = zoomZero ts'


zoomZero :: [a] -> ZoomState a
zoomZero ts = ZoomState { mouse_pos = Nothing
                        , left_press = Nothing
                        , press_complete = Nothing
                        , drag_last = Nothing
                        , ts = ts }

-------------------------------------------------------------

zoomTransform :: RenderablePlus z a => ZoomState z -> Range -> PickFn a -> ZoomState z
zoomTransform tss r f = panTransform (clear ts') r f
  where
    ZoomState { ts = z:_ } = tss
    ts' = maybe tss id transformed
    clear tss = tss { press_complete = Nothing }
    transformed = do
      ps <- press_complete tss
      z' <- transformt ps r f z
      return $ tss { ts = z':(ts tss) }

panTransform :: RenderablePlus z a => ZoomState z -> Range -> PickFn a -> ZoomState z
panTransform tss r f = result
  where
    ZoomState { ts = z:t } = tss
    result = maybe tss id transformed
    transformed = do
      p1 <- drag_last tss
      p2 <- mouse_pos tss
      let z' = maybe z id $ dragTransform (p1,p2) r f z
      return $ tss { drag_last = Just p2, ts = z':t }

-----------------------------------------------------------

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

toZoomableWindow :: (Default z, RenderablePlus z a) => Int -> Int -> EC z () -> IO ()
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

createZoomableWindow :: RenderablePlus z a => z -> Int -> Int -> IO G.Window
createZoomableWindow z windowWidth windowHeight = do
    zooms <- newIORef $ zoomZero [z]
    pickfn <- newIORef nullPickFn :: IO(IORef (PickFn a) )
    window <- G.windowNew
    vbox <- G.vBoxNew False 0
    canvas <- G.drawingAreaNew
    menu <- makeMenu window canvas zooms pickfn z
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.boxPackStart vbox menu G.PackNatural 0
    G.boxPackEnd vbox canvas G.PackGrow 0
    G.set window [G.containerChild G.:= vbox]
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.onExpose canvas $ const $ do
      zs <- readIORef zooms
      range <- dsize canvas
      fn <- readIORef pickfn
      let zs'@ ZoomState { ts =z':_ } = zoomTransform zs range fn
      -- updates the canvas and saves the new PickFn
      fn' <- _updateCanvas False (buildRenderable z') canvas
      writeIORef pickfn fn'
      writeIORef zooms zs'
      drawMouseBox zs' canvas
      G.widgetGetDrawWindow canvas >>= G.drawWindowEndPaint -- manually finish canvas
      return True

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.onMotionNotify canvas True (mouseMotion zooms canvas)
    return window

-- | Draws the mouse selection box
drawMouseBox :: ZoomState a -> G.DrawingArea -> IO Bool
drawMouseBox zoom canvas = do
  case (mouse_pos zoom, left_press zoom) of
    (Just (Point x y), Just (Point x' y')) -> do
      let x'' = round $ min x x'
          y'' = round $ min y y'
          width = round $ abs (x - x')
          height = round $ abs (y - y')

      win <- G.widgetGetDrawWindow canvas
      gc <- gcNew win
      G.drawRectangle win gc False x'' y'' width height
      return True

    -- All other cases we dont draw
    _ -> return True

-- | Saves the mouse position to draw the selection rectangle
mouseMotion :: IORef (ZoomState a) -> G.DrawingArea -> GE.Event -> IO Bool
mouseMotion zooms canvas GE.Motion { GE.eventX = x1, GE.eventY = y1 } = do
  zs <- readIORef zooms
  case (left_press zs, drag_last zs) of
    (Just _, _) -> do
      (w,h) <- dsize canvas
      writeIORef zooms $ zs { mouse_pos = Just $ Point x1 y1 }
      G.widgetQueueDraw canvas
      return True

    (_, Just _) -> do
      writeIORef zooms $ zs { mouse_pos = Just $ Point x1 y1 }
      G.widgetQueueDraw canvas
      return True

    _ -> return True

makeMenu :: RenderablePlus z a => G.Window -> G.DrawingArea -> IORef (ZoomState z) ->
                       IORef (PickFn a) -> z ->  IO G.MenuBar
makeMenu window canvas ref pickfn z = do
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
            sz <- dsize canvas
            fn <- readIORef pickfn
            let ZoomState { ts = r:_ } = zoomTransform zs sz fn
            renderableToFile def p $ toRenderable r
            return ()
      _ -> return () -- shouldn't get here.

    G.widgetDestroy chooser

  quit <- G.menuItemNewWithMnemonic "_Quit"
  G.on quit G.menuItemActivate G.mainQuit
  G.set menu [G.containerChild G.:= save, G.containerChild G.:= quit]

  return menuBar


-- Handles the button events
onButtonEvent :: IORef (ZoomState a) -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dsize canvas
  modifyIORef ref $ \z -> z { left_press = Just (Point (GE.eventX e) (GE.eventY e)) }

  G.widgetQueueDraw canvas
  return True

onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.ReleaseClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dsize canvas
  r <- onButtonRelease ref $ Point (GE.eventX e) (GE.eventY e)
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
  (w,h) <- dsize canvas
  let x = GE.eventX e
      y = GE.eventY e
  modifyIORef ref $ \z -> z { drag_last = Just (Point x y) }
  return True

-- | Pan release
onButtonEvent ref _ (GE.Button{ GE.eventClick = GE.ReleaseClick,
                                       GE.eventButton = GE.MiddleButton }) = do
  modifyIORef ref $ \z -> z { drag_last = Nothing, mouse_pos = Nothing }
  return True

-- | Other mouse events
onButtonEvent _ _ _ = return False
-- End of button events

-- | get the dimentions of the canvas as a pair of Double's
dsize :: G.DrawingArea -> IO (Double, Double)
dsize canvas = do
  (w, h) <- G.widgetGetSize canvas
  return (fromIntegral w, fromIntegral h)

onButtonRelease :: IORef (ZoomState a) -> Point -> IO Bool
onButtonRelease ref p1@(Point x y) = do
  ZoomState { left_press = Just p2@(Point x' y'), ts = ts' } <- readIORef ref
  let
    zs = (zoomZero ts') { press_complete = t }
    t = if dx < 1.0 || dy < 1.0 then Nothing else Just (p1,p2)
    dx = abs (x - x')
    dy = abs (y - y')

  writeIORef ref zs
  return True

updateCanvas :: Renderable a -> G.DrawingArea -> IO Bool
updateCanvas r d = _updateCanvas True r d >> return True

_updateCanvas :: Bool -> Renderable a -> G.DrawingArea -> IO (PickFn a)
_updateCanvas finish chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    regio <- G.regionRectangle $ GE.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    G.drawWindowBeginPaintRegion win regio
    a <- G.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
    if finish
      then G.drawWindowEndPaint win >> return a
      else return a

