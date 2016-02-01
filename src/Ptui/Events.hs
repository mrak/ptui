module Ptui.Events where

import Control.Concurrent.STM (atomically, TQueue)
import Graphics.X11.Types (Window)

data Event = Yes | No deriving Show

setupEventQueue :: TQueue Event -> Window -> IO ()
setupEventQueue q w = pure ()
