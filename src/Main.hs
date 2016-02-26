module Main where

import Ptui.Args
import Ptui.Types
import Ptui.Ptui
import Pt.Pt
import Ui.Xutils
import Ui.Xft

import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import Control.Concurrent (forkIO)
import System.Exit (exitSuccess)

main :: IO ()
main = getArgs >>= runPtui ptui >> exitSuccess

ptui :: Ptui ()
ptui = do
    d <- gets $ display.x11
    w <- gets $ window.x11
    liftIO $ X.clearWindow d w
    uiLoop

uiLoop :: Ptui ()
uiLoop = do
    d <- gets $ display.x11
    fg <- gets $ background.colors
    bg <- gets $ foreground.colors
    drawGrid
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    uiLoop
