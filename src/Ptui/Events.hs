module Ptui.Events where

import Control.Concurrent.STM
import qualified Graphics.UI.GLFW as GLFW

data Event = EventError           !GLFW.Error  !String
           | EventWindowPos       !GLFW.Window !Int !Int
           | EventWindowSize      !GLFW.Window !Int !Int
           | EventWindowClose     !GLFW.Window
           | EventWindowRefresh   !GLFW.Window
           | EventWindowFocus     !GLFW.Window !GLFW.FocusState
           | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
           | EventFramebufferSize !GLFW.Window !Int !Int
           | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | EventCursorPos       !GLFW.Window !Double !Double
           | EventCursorEnter     !GLFW.Window !GLFW.CursorState
           | EventScroll          !GLFW.Window !Double !Double
           | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventChar            !GLFW.Window !Char
           deriving Show

setupEventQueue :: TQueue Event -> GLFW.Window -> IO ()
setupEventQueue q w = do
    GLFW.setErrorCallback             $ Just (\e s ->        atomically $ writeTQueue q $ EventError e s)
    GLFW.setWindowPosCallback       w $ Just (\w' x y ->     atomically $ writeTQueue q $ EventWindowPos w' x y)
    GLFW.setWindowSizeCallback      w $ Just (\w' w h ->     atomically $ writeTQueue q $ EventWindowSize w' w h)
    GLFW.setWindowCloseCallback     w $ Just (\w' ->         atomically $ writeTQueue q $ EventWindowClose w')
    GLFW.setWindowRefreshCallback   w $ Just (\w' ->         atomically $ writeTQueue q $ EventWindowRefresh w')
    GLFW.setWindowFocusCallback     w $ Just (\w' f ->       atomically $ writeTQueue q $ EventWindowFocus w' f)
    GLFW.setWindowIconifyCallback   w $ Just (\w' i ->       atomically $ writeTQueue q $ EventWindowIconify w' i)
    GLFW.setFramebufferSizeCallback w $ Just (\w' w h ->     atomically $ writeTQueue q $ EventFramebufferSize w' w h)
    GLFW.setMouseButtonCallback     w $ Just (\w' b s k ->   atomically $ writeTQueue q $ EventMouseButton w' b s k)
    GLFW.setCursorPosCallback       w $ Just (\w' x y ->     atomically $ writeTQueue q $ EventCursorPos w' x y)
    GLFW.setCursorEnterCallback     w $ Just (\w' c ->       atomically $ writeTQueue q $ EventCursorEnter w' c)
    GLFW.setScrollCallback          w $ Just (\w' x y ->     atomically $ writeTQueue q $ EventScroll w' x y)
    GLFW.setKeyCallback             w $ Just (\w' k c s m -> atomically $ writeTQueue q $ EventKey w' k c s m)
    GLFW.setCharCallback            w $ Just (\w' c ->       atomically $ writeTQueue q $ EventChar w' c)
