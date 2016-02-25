module Ui.Xutils where

import Ptui.Types
import Ptui.Ptui
import Pt.Pt
import Ui.Xft
import Ui.ColorCache
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrender as XR
import Control.Monad.State (get,gets,put)
import Control.Monad.Trans (liftIO)
import Data.Array.IArray (assocs, Array)
import qualified Data.Map.Strict as M

drawGrid :: Ui ()
drawGrid = do
    g <- gets grid
    mapM_ drawRow $ assocs g
    where
        drawRow :: (Int, Array Int (Maybe PtuiCell)) -> Ui ()
        drawRow (y,a) = mapM_ (drawChar y) $ assocs a
        drawChar _ (_,Nothing) = pure ()
        drawChar y (x,Just g) = drawGlyph x y (fg g) (bg g) (glyph g)

drawGlyph :: Int -> Int -> String -> String -> String -> Ui ()
drawGlyph x y' f b s = do
    let y = y' + 1
    xftFont <- gets font
    htext <- gets fontHeight
    wtext <- gets fontWidth
    descent <- gets fontDescent
    dpy <- gets $ display.x11
    win <- gets $ window.x11
    sn <- gets $ screenNumber.x11
    liftIO $ withDrawingColors dpy win f b $ \draw f' b' -> do
                drawXftRect draw b' (x * wtext) (y * htext - htext) wtext htext
                drawXftString draw f' xftFont (x * wtext) (y * htext - descent) s
