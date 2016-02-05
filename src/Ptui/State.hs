module Ptui.State (initState, PtuiState(..)) where

import Ptui.Vt
import Ptui.Settings
import Ptui.Xft

import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import qualified Graphics.X11.Xlib as X
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.IO.Class (MonadIO)
import Data.Array.IArray (Array, array)
import Data.Map.Strict (Map)
import Data.Bits ((.|.))

data PtuiState = PtuiState { cursorPosition :: (Int, Int)
                           , window :: Window
                           , display :: Display
                           , screen :: Screen
                           , screenNumber :: ScreenNumber
                           , font :: AXftFont
                           , fontHeight :: Int
                           , fontWidth :: Int
                           , fontDescent :: Int
                           , grid :: PtuiGrid
                           }

initState :: PtuiSettings -> IO PtuiState
initState settings = do
    (d, w) <- initX settings
    let sn = X.defaultScreen d
    xftFont <- openAXftFont d (X.defaultScreenOfDisplay d) (fontName settings)
    fh <- xft_height xftFont
    fw <- xft_max_advance_width xftFont
    fd <- xft_descent xftFont
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry d w
    let cols = quot (fromIntegral ww - (2 * fromIntegral wb)) fw
    let rows = quot (fromIntegral wh - (2 * fromIntegral wb)) fh
    let rowCells = array (0, cols) [(i,Just PtuiCell {glyph="X",fg="red",bg="white",wide=False})|i<-[0..cols]]
    let g = array (0, rows) [(i,rowCells)|i<-[0..rows]]
    pure PtuiState { cursorPosition = (0,0)
                   , window = w
                   , display = d
                   , screenNumber = sn
                   , screen = X.defaultScreenOfDisplay d
                   , font = xftFont
                   , fontHeight = fh
                   , fontWidth = fw
                   , fontDescent = fd
                   , grid = g
                   }

initX :: PtuiSettings -> IO (X.Display, X.Window)
initX settings = do
    d <- X.openDisplay ""
    let s     = X.defaultScreen d
        black = X.blackPixel d s
    background <- initPixel d (colorbg settings)
    rootw <- X.rootWindow d s
    w <- X.createSimpleWindow d rootw 0 0 100 100 2 background background
    X.setTextProperty d w "ptui" X.wM_NAME
    X.selectInput d w (X.exposureMask .|. X.buttonPressMask)
    X.mapWindow d w
    pure (d,w)

initPixel :: X.Display -> String -> IO X.Pixel
initPixel display color = do
  let colormap = X.defaultColormap display (X.defaultScreen display)
  (apros,real) <- X.allocNamedColor display colormap color
  pure $ X.color_pixel apros

