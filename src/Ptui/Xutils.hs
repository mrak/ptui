module Ptui.Xutils where

import Ptui.Types
import Data.Bits ((.|.))
import qualified Graphics.X11.Xft as Xft
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrender as XR
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Control.Monad.Trans (liftIO)

initPixel :: X.Display -> String -> IO X.Pixel
initPixel display color = do
  let colormap = X.defaultColormap display (X.defaultScreen display)
  (apros,real) <- X.allocNamedColor display colormap color
  pure $ X.color_pixel apros

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

drawGlyph :: Int -> Int -> String -> String -> String -> Ptui ()
drawGlyph x y' f b s = do
    let y = y' + 1
    xftDraw <- gets draw
    xftFont <- gets font
    htext <- gets fontHeight
    wtext <- gets fontWidth
    descent <- gets fontDescent
    dpy <- gets display
    sn <- gets screenNumber
    liftIO $ do
        Xft.withXftColorName dpy (X.defaultVisual dpy sn) (X.defaultColormap dpy sn) b $ \c ->
            Xft.xftDrawRect xftDraw c (x * wtext) (y * htext - htext) wtext htext
        Xft.withXftColorName dpy (X.defaultVisual dpy sn) (X.defaultColormap dpy sn) f $ \c ->
            Xft.xftDrawString xftDraw c xftFont (x * wtext) (y * htext - descent) s
