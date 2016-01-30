module Ptui (ptui) where

import Args
import Settings
import Events
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Colour.SRGB
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Ini as I (readIniFile)

ptui :: Args -> IO ()
ptui args = do
    settings <- readSettings (config args)
    window <- makeWindow 800 600 "ptui"
    queue <- newTQueueIO
    setupEventQueue queue window
    loop settings window queue
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
    if not initialized
       then initError
       else do
           mw <- GLFW.createWindow w h title Nothing Nothing
           case mw of
                Nothing -> initError
                Just w -> do
                    GLFW.makeContextCurrent mw
                    GLFW.swapInterval 1
                    pure w

initError = error "Could not initialize a graphical interface"

toGLColor :: String -> GL.Color4 GL.GLfloat
toGLColor hex = GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1
    where RGB r g b = toSRGB $ sRGB24read hex

draw :: Settings -> GLFW.Window -> IO ()
draw s w = do
    (w, h) <- GLFW.getFramebufferSize w
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.clearColor GL.$= toGLColor (colorbg s)
    GL.clear [GL.ColorBuffer]

processEvents :: TQueue Event -> IO ()
processEvents q = do
    event <- atomically $ tryReadTQueue q
    case event of
         Nothing -> return ()
         Just e -> processEvent e >> processEvents q

processEvent = print

loop :: Settings -> GLFW.Window -> TQueue Event -> IO ()
loop settings window queue = do
    draw settings window
    GLFW.swapBuffers window
    GLFW.waitEvents
    processEvents queue
    shouldClose <- GLFW.windowShouldClose window
    unless shouldClose $ loop settings window queue
