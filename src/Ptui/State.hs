module Ptui.State where

import Ptui.Events
import Control.Concurrent.STM (TQueue)
import qualified Graphics.UI.GLFW as GLFW

data PtuiState = PtuiState { cursorPosition :: (Int, Int)
                           , window :: GLFW.Window
                           , queue :: TQueue Event
                           }
