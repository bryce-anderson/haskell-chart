-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk.Interactive
-- Copyright   :  (c) Bryce Anderson 2014
-- License     :  BSD-style (see chart/COPYRIGHT)

{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Rendering.Chart.Gtk.Interactive (
  createInteractiveWindow,
  updateCanvas
) where


import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import qualified Graphics.UI.Gtk.Selectors.FileChooserDialog as FC
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk.Gdk.GC

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Renderable

import Data.IORef
import Data.Default.Class
import Control.Applicative
import Data.Maybe


data RState = forall f . RState {
    r :: Renderable f,
    pickfn :: PickFn f,
    selectT :: (Point,Point) -> Range -> PickFn f -> Maybe InteractiveElement,
    dragT :: (Point,Point) -> Range -> PickFn f -> Maybe InteractiveElement
}

-- | bits for generating/modifying/querying the state of zoom
data ZoomState =  ZoomState { mouse_pos :: Maybe Point
                            , left_press :: Maybe Point
                            , press_complete :: Maybe (Point,Point)
                            , drag_last :: Maybe Point
                            , ts :: [RState] }

-- | Remove a zoom from the stack
popZoom :: ZoomState -> ZoomState
popZoom h@(ZoomState { ts = [_] }) = h
popZoom h@(ZoomState { ts = (t:ts') }) = zoomZero ts'

-- | Add a state to the stack
pushZoom :: ZoomState -> RState -> ZoomState
pushZoom ZoomState{ts=ts'} rs = zoomZero (rs:ts')

zoomZero :: [RState] -> ZoomState
zoomZero ts = ZoomState { mouse_pos = Nothing
                        , left_press = Nothing
                        , press_complete = Nothing
                        , drag_last = Nothing
                        , ts = ts }

lastRenderable :: ZoomState -> Renderable ()
lastRenderable ZoomState{ts=r:_} = r'
  where
    r' = case r of
      RState { r = re } -> setPickFn nullPickFn re

-------------------------------------------------------------
-- | computes a possible new InteractiveElement
zoomTransform :: ZoomState -> Range -> Maybe (Bool,InteractiveElement)
zoomTransform tss r = ts'
  where
    ts' = case head $ ts tss of
      RState { pickfn = pf, selectT = select, dragT = drag } -> ts' where
        ts' = dragTransformed <|> panTransformed
        dragTransformed = do
          ps <- press_complete tss
          r <- select ps r pf
          return (True,r)

        panTransformed = do
          p1 <- drag_last tss
          p2 <- mouse_pos tss
          r <- drag (p1,p2) r pf
          return (False,r)

-----------------------------------------------------------
-- | helper methods for rendering
-----------------------------------------------------------
resetExpose :: InteractiveElement -> G.DrawingArea -> IORef (Maybe ZoomState) -> IO ()
resetExpose el canvas ref = case el of
  InteractiveElement { renderable = r
                     , selectTransform = select
                     , dragTransform = drag } -> do
    fn <- _updateCanvas False r canvas
    let zs = RState { r = r, pickfn = fn, selectT = select, dragT = drag }
    writeIORef ref $ Just $ zoomZero [zs]

onExpose :: ZoomState -> G.DrawingArea -> IORef (Maybe ZoomState) -> IO ()
onExpose zs@ZoomState{ ts = _:ts' } canvas ref = do
  range <- dsize canvas
  case zoomTransform zs range of
    -- new rendering, need to save it and maybe push the ref stack
    Just (save, InteractiveElement { renderable = r
                             , selectTransform = select
                             , dragTransform = drag }) -> do
      fn' <- _updateCanvas False r canvas
      let rs = RState { r = r, pickfn = fn', selectT = select, dragT = drag }
          d' = drag_last zs >> mouse_pos zs
          zs' = if save then pushZoom zs rs
                else zs { drag_last = d', ts = rs:ts' }
      writeIORef ref $ Just zs'

    -- No transform, maybe a mouse box?
    Nothing -> do
      let ZoomState { ts = r:_ } = zs
          rend :: Renderable ()
          rend = case r of RState { r = r' } -> setPickFn nullPickFn r'
      _updateCanvas False rend canvas
      drawMouseBox zs canvas
      return ()

-----------------------------------------------------------

-- | Creates a GTK window that supports selection and dragging
createInteractiveWindow :: InteractiveElement -> Int -> Int -> IO G.Window
createInteractiveWindow el windowWidth windowHeight = do
    zooms <- newIORef Nothing
    window <- G.windowNew
    vbox <- G.vBoxNew False 0
    canvas <- G.drawingAreaNew
    menu <- makeMenu window canvas zooms
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.boxPackStart vbox menu G.PackNatural 0
    G.boxPackEnd vbox canvas G.PackGrow 0
    G.set window [G.containerChild G.:= vbox]
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.onExpose canvas $ const $ do
      maybezs <- readIORef zooms
      case maybezs of
        Just zs -> onExpose zs canvas zooms
        Nothing -> resetExpose el canvas zooms
      G.widgetGetDrawWindow canvas >>= G.drawWindowEndPaint -- manually finish canvas
      return True

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.onMotionNotify canvas True (mouseMotion zooms canvas)
    return window

-- | Draws the mouse selection box
drawMouseBox :: ZoomState -> G.DrawingArea -> IO Bool
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
mouseMotion :: IORef (Maybe ZoomState) -> G.DrawingArea -> GE.Event -> IO Bool
mouseMotion zooms canvas GE.Motion { GE.eventX = x1, GE.eventY = y1 } = do
  zs <- readIORef zooms
  case (zs >>= left_press, zs >>= drag_last) of
    (Just _, _) -> do
      writeIORef zooms $ Just $ (fromJust zs) { mouse_pos = Just $ Point x1 y1 }
      G.widgetQueueDraw canvas
      return True

    (_, Just _) -> do
      writeIORef zooms $ Just $ (fromJust zs) { mouse_pos = Just $ Point x1 y1 }
      G.widgetQueueDraw canvas
      return True

    _ -> return True

-- | construct a MenuBar
makeMenu :: G.Window -> G.DrawingArea -> IORef (Maybe ZoomState) ->  IO G.MenuBar
makeMenu window canvas ref = do
  -- File menu
  menuBar <- G.menuBarNew
  filemenu <- G.menuItemNewWithMnemonic "_File"
  G.set menuBar [G.containerChild G.:= filemenu]
  fmenu <- G.menuNew
  filemenu `G.menuItemSetSubmenu` fmenu
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
        maybepath <- G.fileChooserGetFilename chooser
        maybezs <- readIORef ref
        let maybesave :: Maybe (IO ())
            maybesave = do
              ZoomState { ts = r:_ } <- maybezs
              p <- maybepath
              return $ do
                sz <- dsize canvas
                case r of RState { r = r } -> renderableToFile def p r >> return ()

        maybe (return ()) id maybesave
      _ -> return () -- shouldn't get here.

    G.widgetDestroy chooser

  quit <- G.menuItemNewWithMnemonic "_Quit"
  G.on quit G.menuItemActivate G.mainQuit
  G.set fmenu [G.containerChild G.:= save, G.containerChild G.:= quit]

  -- Chart menu
  initialState <- readIORef ref
  chartmenu <- G.menuItemNewWithMnemonic "_Chart"
  G.set menuBar [G.containerChild G.:= chartmenu]
  cmenu <- G.menuNew
  chartmenu `G.menuItemSetSubmenu` cmenu
  reset <- G.menuItemNewWithMnemonic "_Reset"
  reset `G.on` G.menuItemActivate $ do
    writeIORef ref initialState
    G.widgetQueueDraw canvas

  G.set cmenu [G.containerChild G.:= reset]

  return menuBar


-- Handles the button events
onButtonEvent :: IORef (Maybe ZoomState) -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.LeftButton }) = do
  modifyIORef ref $ fmap modify
  G.widgetQueueDraw canvas
  return True
    where modify z = z { left_press = Just (Point (GE.eventX e) (GE.eventY e)) }


onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.ReleaseClick,
                                        GE.eventButton = GE.LeftButton }) = do
  r <- onButtonRelease ref $ Point (GE.eventX e) (GE.eventY e)
  G.widgetQueueDraw canvas
  return r

onButtonEvent ref canvas (GE.Button { GE.eventClick = GE.SingleClick,
                                      GE.eventButton = GE.RightButton }) = do
  modifyIORef ref $ fmap popZoom
  G.widgetQueueDraw canvas
  return True

-- | Pan click
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.MiddleButton }) = do
  let x = GE.eventX e
      y = GE.eventY e
  modifyIORef ref $ fmap $ \z -> z { drag_last = Just (Point x y) }
  return True

-- | Pan release
onButtonEvent ref _ (GE.Button{ GE.eventClick = GE.ReleaseClick,
                                       GE.eventButton = GE.MiddleButton }) = do
  modifyIORef ref $ fmap $ \z -> z { drag_last = Nothing, mouse_pos = Nothing }
  return True

-- | Other mouse events
onButtonEvent _ _ _ = return False
-- End of button events

-- | get the dimentions of the canvas as a pair of Double's
dsize :: G.WidgetClass w => w -> IO (Double, Double)
dsize canvas = do
  (w, h) <- G.widgetGetSize canvas
  return (fromIntegral w, fromIntegral h)

onButtonRelease :: IORef (Maybe ZoomState) -> Point -> IO Bool
onButtonRelease ref p1@(Point x y) = readIORef ref >>= go
  where
    go (Just ZoomState { left_press = Just p2@(Point x' y'), ts = ts' }) = do
      let zs = (zoomZero ts') { press_complete = t }
          t  = if dx < 1.0 || dy < 1.0 then Nothing else Just (p1,p2)
          dx = abs (x - x')
          dy = abs (y - y')

      writeIORef ref $ Just zs
      return True
    go _ = return True

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

