module Ptui (ptui) where

import Args
import Settings
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Control.Concurrent
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Ini as I (readIniFile)

ptui :: Args -> IO ()
ptui args = do
    settings <- readSettings (config args)
    window <- makeWindow 800 600 "ptui"

    threadDelay 10000000
    GLFW.destroyWindow window
    GLFW.terminate

readSettings :: FilePath -> IO Settings
readSettings fp = do
    exists <- doesFileExist fp
    if exists
       then I.readIniFile fp >>= pure . either (const defaultSettings) fromINI
       else warn ("Configuration file " ++ fp ++ " does not exist. Using default settings.") >> pure defaultSettings

warn :: String -> IO ()
warn = hPutStrLn stderr

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
