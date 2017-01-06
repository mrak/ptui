module Ptui.Ptui where

import Ptui.Args
import Ptui.Config (fromConfig)
import Ptui.Types
import Pt.Ioctl (setTerminalSize)
import qualified Pt.Commands as PtCommands (stream)
import qualified Pt.Spawn as Spawn (shell)
import qualified Ui.Commands as UiCommands (process)
import Ui.Xft
import Ui.Xutils
import Ui.ColorCache

import Lens.Simple
import Control.Monad.State (runStateT)
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import Graphics.X11.Xlib.Misc (lockDisplay,unlockDisplay)
import qualified Graphics.X11.Xlib as X
import Data.Array.IArray (assocs,Array,array,bounds)
import Data.Map.Strict (Map)
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Monad (when,void)
import Control.Monad.Trans (liftIO)
import System.Exit (exitSuccess,exitFailure)
import System.Posix.IO.ByteString (dupTo,stdInput)
import System.Posix.Process (getProcessID,exitImmediately)
import System.Posix.Process.Internals (ProcessStatus(Exited))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO as IO
import System.Posix.Signals ( SignalInfo
                            , SignalSpecificInfo(NoSignalSpecificInfo)
                            , Handler(Catch,CatchInfo)
                            , signalProcessGroup
                            , siginfoStatus
                            , siginfoSpecific
                            , installHandler
                            , sigHUP
                            , sigCHLD
                            , sigINT
                            , siginfoPid
                            )
import System.Posix.Terminal (openPseudoTerminal)

import System.Posix.Types (ProcessID)

runPtui :: Ptui p -> Args -> IO (p, PtuiState)
runPtui p a = do
    settings <- fromConfig (config a)
    state <- initPtui settings
    runStateT (run p) state

nextCommand :: Ptui Command
nextCommand = use channel >>= liftIO . atomically . readTQueue

initPtui :: PtuiConfig -> IO PtuiState
initPtui settings = do
    chan <- atomically newTQueue
    x <- initX settings
    ft <- fetchFont x (settings^.cfont)
    pid <- getProcessID
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry (x^.display) (x^.window)
    let cols = quot (fromIntegral ww - (2 * fromIntegral wb)) (ft^.width)
    let rows = quot (fromIntegral wh - (2 * fromIntegral wb)) (ft^.height)
    let g = array (0, rows * cols) [(i,PtuiCell {_glyph='X',_fg="red",_bg="white",_wide=False,_dirty=True})|i<-[0..(cols*rows)]]
    pure PtuiState { _cursorPosition = (0,0)
                   , _x11 = x
                   , _colors = settings^.ccolors
                   , _font = ft
                   , _grid = PtuiGrid rows cols g
                   , _channel = chan
                   , _childPid = pid
                   }

ptui :: Ptui ()
ptui = do
    d <- use $ x11.display
    w <- use $ x11.window
    chan <- use channel

    p <- liftIO $ do
        (master,slave) <- openPseudoTerminal
        dupTo master stdInput
        forkIO $ PtCommands.stream master chan
        pid <- Spawn.shell slave
        installHandler sigCHLD (CatchInfo $ sigchld pid) Nothing
        installHandler sigINT (Catch $ sigint pid) Nothing
        setTerminalSize master 54 64 120 120
        pure pid
    childPid .= p
    loop

loop :: Ptui ()
loop = do
    UiCommands.process
    c <- use channel
    pid <- use childPid
    cmd <- nextCommand
    case cmd of
            WindowClose -> liftIO $ sigint pid
            X11Event _ -> liftIO $ sigint pid
            _ -> liftIO $ printCmd cmd
    drawGrid
    loop


sigint :: ProcessID -> IO ()
sigint pid = signalProcessGroup sigHUP pid >> exitSuccess

sigchld :: ProcessID -> SignalInfo -> IO ()
sigchld pid si = case siginfoSpecific si of
                      NoSignalSpecificInfo -> pure ()
                      sci -> when (siginfoPid sci == pid) exit
                          where exit = case siginfoStatus sci of
                                            Exited c -> exitImmediately c
                                            _ -> exitFailure

printCmd :: Command -> IO ()
printCmd (Output c) = putChar c
printCmd c = print c
