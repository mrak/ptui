module Lib (ptui) where

import Args
import Settings
import Control.Monad (when)
import Control.Concurrent
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Ini as I (readIniFile)

ptui :: Args -> IO ()
ptui a = do
    configContents <- I.readIniFile (config a)
    let settings = either error fromINI configContents

    window <- makeWindow 800 600 "ptui"
    threadDelay 10000000
    GLFW.destroyWindow window
    GLFW.terminate


makeWindow :: Int -> Int -> String -> IO GLFW.Window
makeWindow w h title = do
    initialized <- GLFW.init
    if initialized
       then do
            mw <- GLFW.createWindow w h title Nothing Nothing
            case mw of
                Nothing -> error "Could not create a window"
                Just w -> do
                    GLFW.makeContextCurrent mw
                    pure w
       else error "Could not initialize GLFW"
