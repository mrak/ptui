{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ptui.Ptui (ptui) where

import Ptui.Args
import Ptui.Settings
import Ptui.Events
import Ptui.State
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks, ReaderT, runReaderT, MonadReader)
import Control.Monad.State (gets, StateT, runStateT, MonadState)
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Colour.SRGB
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Ini as I (readIniFile)

newtype Ptui a = Ptui { run :: ReaderT PtuiSettings (StateT PtuiState IO) a
                      } deriving (Functor, Applicative, Monad, MonadIO, MonadReader PtuiSettings, MonadState PtuiState)

runPtui :: Ptui a -> Args -> IO ()
runPtui p a = do
    w <- makeWindow 800 600 "ptui"
    q <- newTQueueIO
    settings <- readSettings (config a)
    let state = PtuiState { cursorPosition = (0,0)
                          , window = w
                          , queue = q
                          }
    setupEventQueue q w
    runStateT (runReaderT (run p) settings) state
    GLFW.destroyWindow w
    GLFW.terminate

ptui :: Args -> IO ()
ptui = runPtui loop
{-ptui :: Args -> IO ()-}
{-ptui args = do-}
    {-settings <- readSettings (config args)-}
    {-window <- makeWindow 800 600 "ptui"-}
    {-queue <- newTQueueIO-}
    {-setupEventQueue queue window-}
    {-loop settings window queue-}
    {-GLFW.destroyWindow window-}
    {-GLFW.terminate-}

readSettings :: FilePath -> IO PtuiSettings
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

draw :: Ptui ()
draw = do
    bg <- asks colorbg
    w <- gets window
    liftIO $ do
        (w, h) <- GLFW.getFramebufferSize w
        GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        GL.clearColor GL.$= toGLColor bg
        GL.clear [GL.ColorBuffer]

processEvents :: Ptui ()
processEvents = do
    q <- gets queue
    event <- liftIO $ atomically $ tryReadTQueue q
    case event of
         Nothing -> pure ()
         Just e -> processEvent e >> processEvents

processEvent :: Event -> Ptui ()
processEvent e = liftIO $ print e

loop :: Ptui ()
loop = do
    w <- gets window
    draw
    liftIO $ GLFW.swapBuffers w >> GLFW.waitEvents
    processEvents
    shouldClose <- liftIO $ GLFW.windowShouldClose w
    unless shouldClose loop
