module Main where

import Ptui.Ptui
import Ptui.Args
import Ptui.State
import Pt.Pt
import Ui.Xutils
import Ui.Xft
import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import System.Exit (exitSuccess)

main :: IO ()
main = getArgs >>= ptui

ptui :: Args -> IO ()
ptui a = runPtui loop a >> exitSuccess

loop :: Ptui ()
loop = do
    d <- gets $ display.x11
    w <- gets $ windowId.x11
    fg <- gets $ background.colors
    bg <- gets $ foreground.colors
    liftIO $ X.clearWindow d w
    drawGrid
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    loop
