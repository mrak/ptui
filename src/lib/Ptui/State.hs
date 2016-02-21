module Ptui.State (initState, PtuiState(..), PtuiX11(..), PtuiColors(..)) where

import Pt.Pt
import Ptui.Config as C
import Ui.Xft

import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import qualified Graphics.X11.Xlib as X
import Data.Array.IArray (Array, array)
import Data.Map.Strict (Map)
import Data.Bits ((.|.))

data PtuiX11 = PtuiX11
               { display :: Display
               , windowId :: Window
               , screen :: Screen
               , screenNumber :: ScreenNumber
               }

data PtuiState = PtuiState { cursorPosition :: (Int, Int)
                           , x11 :: PtuiX11
                           , colors :: PtuiColors
                           , font :: AXftFont
                           , fontHeight :: Int
                           , fontWidth :: Int
                           , fontDescent :: Int
                           , grid :: PtuiGrid
                           }

initState :: PtuiConfig -> IO PtuiState
initState settings = do
    (d, w) <- initX settings
    let sn = X.defaultScreen d
    xftFont <- openAXftFont d (X.defaultScreenOfDisplay d) (C.font settings)
    fh <- xft_height xftFont
    fw <- xft_max_advance_width xftFont
    fd <- xft_descent xftFont
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry d w
    let cols = quot (fromIntegral ww - (2 * fromIntegral wb)) fw
    let rows = quot (fromIntegral wh - (2 * fromIntegral wb)) fh
    let rowCells = array (0, cols) [(i,Just PtuiCell {glyph="X",fg="red",bg="white",wide=False})|i<-[0..cols]]
    let g = array (0, rows) [(i,rowCells)|i<-[0..rows]]
    let x11State = PtuiX11 { display = d
                           , windowId = w
                           , screenNumber = sn
                           , screen = X.defaultScreenOfDisplay d
                           }
    pure PtuiState { cursorPosition = (0,0)
                   , x11 = x11State
                   , Ptui.State.colors = C.colors settings
                   , Ptui.State.font = xftFont
                   , fontHeight = fh
                   , fontWidth = fw
                   , fontDescent = fd
                   , grid = g
                   }

initX :: PtuiConfig -> IO (X.Display, X.Window)
initX settings = do
    d <- X.openDisplay ""
    let s     = X.defaultScreen d
        black = X.blackPixel d s
    bg <- initPixel d (background.C.colors$settings)
    rootw <- X.rootWindow d s
    w <- X.createSimpleWindow d rootw 0 0 100 100 2 bg bg
    X.setTextProperty d w (title $ window settings) X.wM_NAME
    X.setTextProperty d w (clazz $ window settings) X.wM_NAME
    X.selectInput d w (X.exposureMask .|. X.buttonPressMask)
    X.mapWindow d w
    pure (d,w)

initPixel :: X.Display -> String -> IO X.Pixel
initPixel display color = do
  let colormap = X.defaultColormap display (X.defaultScreen display)
  (apros,real) <- X.allocNamedColor display colormap color
  pure $ X.color_pixel apros

