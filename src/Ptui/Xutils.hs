module Ptui.Xutils where

import Ptui.Types
import Data.Bits ((.|.))
import qualified Graphics.X11.Xft as Xft
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrender as XR
import Control.Monad.Reader (asks)
import Control.Monad.State (get,gets,put)
import Control.Monad.Trans (liftIO)
import Data.Array.IArray (assocs, Array)
import qualified Data.Map.Strict as M

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

drawGrid :: Ptui ()
drawGrid = do
    g <- gets grid
    mapM_ (drawRow) $ assocs g
    where
        drawRow :: (Int, Array Int (Maybe PtuiCell)) -> Ptui ()
        drawRow (y,a) = mapM_ (drawChar y) $ assocs a
        drawChar _ (_,Nothing) = pure ()
        drawChar y (x,Just g) = drawGlyph (x) (y) (fg g ) (bg g) (glyph g)

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
    ccache <- gets colorCache
    bgc <- liftIO $ case M.lookup b ccache of
                            Nothing -> print "alloc" >> Xft.withXftColorName dpy (X.defaultVisual dpy sn) (X.defaultColormap dpy sn) b pure
                            Just c -> pure c
    fgc <- liftIO $ case M.lookup f ccache of
                            Nothing -> Xft.withXftColorName dpy (X.defaultVisual dpy sn) (X.defaultColormap dpy sn) f pure
                            Just c -> pure c
    let ccache' = M.insert b bgc ccache
    let ccache'' = M.insert f fgc ccache'
    state <- get
    put state {colorCache = ccache''}
    liftIO $ do
        Xft.xftDrawRect xftDraw bgc (x * wtext) (y * htext - htext) wtext htext
        Xft.xftDrawString xftDraw fgc xftFont (x * wtext) (y * htext - descent) s