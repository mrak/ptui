module Ui.Commands where

import Ptui.Types
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import Control.Concurrent.STM (atomically,TQueue,writeTQueue,readTQueue)
import Control.Monad (when)
import Data.Bits ((.|.))
import Lens.Simple
import Control.Monad.Trans (liftIO)

process :: Ptui ()
process = do
    d <- use $ x11.display
    w <- use $ x11.window
    q <- use channel
    liftIO $ do
        X.selectInput d w (X.exposureMask .|. X.buttonPressMask)
        X.allocaXEvent $ \e -> do
            evs <- X.pending d
            when (evs > 0) $ do
                X.nextEvent d e
                ev <- XE.getEvent e
                atomically $ writeTQueue q $ X11Event $ XE.eventName ev
