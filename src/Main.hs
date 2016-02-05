{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Ptui.Ptui
import Ptui.Args
import Ptui.Settings
import Ptui.State
import Ptui.Vt
import Ptui.Xutils
import Ptui.Xft
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State (gets, runStateT)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import System.Exit (exitSuccess)
import qualified Data.Ini as I (readIniFile)
import Data.Array.IArray (array)
import Data.Map.Strict (Map, fromList)

main :: IO ()
main = getArgs >>= ptui

ptui :: Args -> IO ()
ptui a = runPtui loop a >> exitSuccess

loop :: Ptui ()
loop = do
    d <- gets display
    w <- gets window
    fg <- asks colorbg
    bg <- asks colorfg
    liftIO $ X.clearWindow d w
    drawGrid
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    loop
