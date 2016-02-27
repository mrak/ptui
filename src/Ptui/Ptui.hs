module Ptui.Ptui (runPtui) where

import Ptui.Args
import Ptui.Config (fromConfig)
import Ptui.Types
import Ui.Xft

import Lens.Simple
import Control.Monad.State (runStateT)
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import qualified Graphics.X11.Xlib as X
import Data.Array.IArray (Array, array)
import Data.Map.Strict (Map)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)

runPtui :: Ptui p -> Args -> IO (p, PtuiState)
runPtui p a = do
    settings <- fromConfig (config a)
    state <- initState settings
    runStateT (run p) state

initState :: PtuiConfig -> IO PtuiState
initState settings = do
    x <- initX settings
    xftFont <- openAXftFont (x^.display) (x^.screen) (settings^.cfont)
    fh <- xft_height xftFont
    fw <- xft_max_advance_width xftFont
    fd <- xft_descent xftFont
    chan <- atomically newTQueue
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry (x^.display) (x^.window)
    let cols = quot (fromIntegral ww - (2 * fromIntegral wb)) fw
    let rows = quot (fromIntegral wh - (2 * fromIntegral wb)) fh
    let rowCells = array (0, cols) [(i,Just PtuiCell {_glyph="X",_fg="red",_bg="white",_wide=False})|i<-[0..cols]]
    let g = array (0, rows) [(i,rowCells)|i<-[0..rows]]
    pure PtuiState { _cursorPosition = (0,0)
                   , _x11 = x
                   , _colors = settings^.ccolors
                   , _font = xftFont
                   , _fontHeight = fh
                   , _fontWidth = fw
                   , _fontDescent = fd
                   , _grid = g
                   , _channel = chan
                   }

initX :: PtuiConfig -> IO PtuiX11
initX settings = do
    d <- X.openDisplay ""
    let sn    = X.defaultScreen d
        black = X.blackPixel d sn
    bg <- initPixel d (settings^.ccolors.background)
    rootw <- X.rootWindow d sn
    w <- X.createSimpleWindow d rootw 0 0 100 100 2 bg bg
    X.setTextProperty d w (settings^.cwindow.title) X.wM_NAME
    X.setTextProperty d w (settings^.cwindow.clazz) X.wM_NAME
    X.mapWindow d w
    pure PtuiX11 { _display = d
                 , _window = w
                 , _screenNumber = sn
                 , _screen = X.defaultScreenOfDisplay d
                 }

initPixel :: Display -> String -> IO X.Pixel
initPixel display color = do
  let colormap = X.defaultColormap display (X.defaultScreen display)
  (apros,real) <- X.allocNamedColor display colormap color
  pure $ X.color_pixel apros
