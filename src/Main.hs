module Main where

import Ptui.Config (term)
import Pt.StateMachine
import Ptui.Args
import Ptui.Types
import Ptui.Ptui
import Ui.Xutils
import Ui.Xft
import Pt.Ioctl (setControllingTerminal,setTerminalSize)

import Lens.Simple
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import Control.Concurrent (forkIO)
import System.Exit (exitSuccess,exitFailure)
import Control.Concurrent.STM (atomically,TQueue,writeTQueue,readTQueue)
import System.Posix.User
import System.Posix.Signals as Signals
import System.Posix.Process
import System.Posix.Process.Internals
import System.Posix.Types (Fd,ProcessID)
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.Posix.IO.ByteString
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO as IO
import System.Posix.Terminal (openPseudoTerminal)

main :: IO ()
main = getArgs >>= runPtui ptui >> exitSuccess

ptui :: Ptui ()
ptui = do
    d <- use $ x11.display
    w <- use $ x11.window
    chan <- use channel

    p <- liftIO $ do
        (master,slave) <- openPseudoTerminal
        dupTo master stdInput
        forkIO $ ptCommands master chan
        forkIO $ uiCommands d w chan
        pid <- spawnShell slave
        installHandler sigCHLD (CatchInfo $ sigchld pid) Nothing
        installHandler sigINT (Catch $ sigint pid) Nothing
        setTerminalSize master 54 64 120 120
        X.clearWindow d w
        pure pid
    childPid .= p
    loop

printCmd :: Command -> IO ()
printCmd (Output c) = putChar c
printCmd c = print c

loop :: Ptui ()
loop = do
    drawGrid
    c <- use channel
    pid <- use childPid
    cmd <- nextCommand
    case cmd of
            WindowClose -> liftIO $ sigint pid
            X11Event _ -> liftIO $ sigint pid
            _ -> liftIO $ printCmd cmd
    loop

ptCommands :: Fd -> TQueue Command -> IO ()
ptCommands f c = input f >>= pure . runFSM >>= mapM_ (atomically . writeTQueue c)
    where input fd = unbuffer fd >>= B.hGetContents
          unbuffer fd = do
              h <- fdToHandle fd
              IO.hSetBinaryMode h True
              IO.hSetBuffering h $ IO.BlockBuffering Nothing
              pure h

sigint :: ProcessID -> IO ()
sigint pid = signalProcessGroup sigHUP pid >> exitSuccess

sigchld :: ProcessID -> SignalInfo -> IO ()
sigchld pid si = case siginfoSpecific si of
                      NoSignalSpecificInfo -> pure ()
                      sci -> when (siginfoPid sci == pid) exit
                          where exit = case siginfoStatus sci of
                                            Exited c -> exitImmediately c
                                            _ -> exitFailure

spawnShell :: Fd -> IO ProcessID
spawnShell fd = do
    sh <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
    spawnCmd sh [] fd

spawnCmd :: String -> [String] -> Fd -> IO ProcessID
spawnCmd cmd args fd = forkProcess $ do
    createSession
    dupTo fd stdInput
    dupTo fd stdOutput
    dupTo fd stdError
    setControllingTerminal fd
    closeFd fd

    udata <- getRealUserID >>= getUserEntryForID
    home <- lookupEnv "HOME"
    sh <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
    unsetEnv "COLUMNS"
    unsetEnv "LINES"
    unsetEnv "TERMCAP"
    setEnv "USER" $ userName udata
    setEnv "SHELL" sh
    setEnv "TERM" term
    setEnv "LOGNAME" $ userName udata

    installHandler sigCHLD Signals.Default Nothing
    installHandler sigHUP Signals.Default Nothing
    installHandler sigINT Signals.Default Nothing
    installHandler sigQUIT Signals.Default Nothing
    installHandler sigTERM Signals.Default Nothing
    installHandler sigALRM Signals.Default Nothing

    executeFile cmd True args Nothing
    pure ()
