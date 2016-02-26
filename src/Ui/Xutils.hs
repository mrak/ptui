module Ui.Xutils where

import Ptui.Types
import Ptui.Ptui
import Ui.Xft
import Ui.ColorCache

import Lens.Simple
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrender as XR
import Control.Monad.Trans (liftIO)
import Data.Array.IArray (assocs, Array)
import qualified Data.Map.Strict as M

drawGrid :: Ptui ()
drawGrid = do
    g <- use grid
    mapM_ drawRow $ assocs g
    where
        drawRow :: (Int, Array Int (Maybe PtuiCell)) -> Ptui ()
        drawRow (y,a) = mapM_ (drawChar y) $ assocs a
        drawChar _ (_,Nothing) = pure ()
        drawChar y (x,Just g) = drawGlyph x y (g^.fg) (g^.bg) (g^.glyph)

drawGlyph :: Int -> Int -> String -> String -> String -> Ptui ()
drawGlyph x y' f b s = do
    let y = y' + 1
    xftFont <- use font
    htext <- use fontHeight
    wtext <- use fontWidth
    descent <- use fontDescent
    dpy <- use $ x11.display
    win <- use $ x11.window
    sn <- use $ x11.screenNumber
    liftIO $ withDrawingColors dpy win f b $ \draw f' b' -> do
                drawXftRect draw b' (x * wtext) (y * htext - htext) wtext htext
                drawXftString draw f' xftFont (x * wtext) (y * htext - descent) s
