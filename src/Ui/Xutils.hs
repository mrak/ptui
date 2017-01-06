module Ui.Xutils where

import Ptui.Types
import Ui.Xft
import Ui.ColorCache

import Lens.Simple
import Data.Array.IArray (assocs,array)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.X11.Xrender as XR
import Graphics.X11.Xlib.Atom (internAtom)
import Graphics.X11.Xlib.Misc (setWMProtocols,lockDisplay,unlockDisplay)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Strict as M
import Control.Concurrent (threadDelay)

fetchFont :: PtuiX11 -> String -> IO PtuiFont
fetchFont x name = do
    xftFont <- openAXftFont (x^.display) (x^.screen) name
    fh <- xft_height xftFont
    fw <- xft_max_advance_width xftFont
    fd <- xft_descent xftFont
    pure PtuiFont { _face = xftFont
                  , _width = fw
                  , _height = fh
                  , _descent = fd
                  }

isWMDeleteWinEvent :: XE.Event -> IO Bool
isWMDeleteWinEvent e = pure True

initPixel :: X.Display -> String -> IO X.Pixel
initPixel display color = do
  let colormap = X.defaultColormap display (X.defaultScreen display)
  (apros,real) <- X.allocNamedColor display colormap color
  pure $ X.color_pixel apros

initX :: PtuiConfig -> IO PtuiX11
initX settings = do
    X.initThreads
    d <- X.openDisplay ""
    let sn    = X.defaultScreen d
        black = X.blackPixel d sn
    bg <- initPixel d (settings^.ccolors.background)
    rootw <- X.rootWindow d sn
    w <- X.createSimpleWindow d rootw 0 0 100 100 2 bg bg
    a <- internAtom d "WM_DELETE_WINDOW" False
    setWMProtocols d w [a]
    X.setTextProperty d w (settings^.cwindow.title) X.wM_NAME
    X.setTextProperty d w (settings^.cwindow.clazz) X.wM_NAME
    X.mapWindow d w
    X.clearWindow d w
    pure PtuiX11 { _display = d
                 , _window = w
                 , _screenNumber = sn
                 , _screen = X.defaultScreenOfDisplay d
                 }

drawGrid :: Ptui ()
drawGrid = do
    rs <- use $ grid.rows
    cs <- use $ grid.cols
    g <- use $ grid.cells
    liftIO . print $ "rows: " ++ show rs ++ " cols: " ++ show cs
    newCells <- mapM drawCell (assocs g)
    grid.cells .= array (0,rs*cs) newCells

drawCell :: (Int,PtuiCell) -> Ptui (Int,PtuiCell)
drawCell t@(i,cell) | not (cell^.dirty) = pure t
                    | otherwise = do
    rs <- use $ grid.rows
    cs <- use $ grid.cols
    let x = mod i cs
    let y = quot i cs + 1
    let f = cell^.fg
    let b = cell^.bg
    let c = cell^.glyph
    liftIO . print $ "i: " ++ show i ++ " x: " ++ show x ++ " y: " ++ show y
    xftFont <- use $ font.face
    htext <- use $ font.height
    wtext <- use $ font.width
    descent <- use $ font.descent
    dpy <- use $ x11.display
    win <- use $ x11.window
    sn <- use $ x11.screenNumber
    liftIO $ withDrawingColors dpy win f b $ \draw f' b' -> do
        drawXftRect draw b' (x * wtext) (y * htext - htext) wtext htext
        drawXftString draw f' xftFont (x * wtext) (y * htext - descent) [c]
    pure (i,cell { _dirty = False })
