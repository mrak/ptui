module Ui.Xutils where

import Ptui.Types
import Ui.Xft
import Ui.ColorCache

import Lens.Simple
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.X11.Xrender as XR
import Graphics.X11.Xlib.Atom (internAtom)
import Graphics.X11.Xlib.Misc (setWMProtocols)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Strict as M
import Control.Monad (forever)
import Data.Bits ((.|.))
import Control.Concurrent.STM (atomically,TQueue,writeTQueue,readTQueue)

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

uiCommands :: X.Display -> X.Window -> TQueue Command -> IO ()
uiCommands d w q = do
    X.selectInput d w (X.exposureMask .|. X.buttonPressMask)
    X.allocaXEvent $ \e -> forever $ do
        X.sync d True
        X.nextEvent d e
        ev <- XE.getEvent e
        atomically $ writeTQueue q $ X11Event $ XE.eventName ev


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
    pure PtuiX11 { _display = d
                 , _window = w
                 , _screenNumber = sn
                 , _screen = X.defaultScreenOfDisplay d
                 }
