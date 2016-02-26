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
import Control.Monad (when,forever)
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

    liftIO $ do
        (master,slave) <- openPseudoTerminal
        forkIO $ pt master chan
        forkIO $ ui d chan
        pid <- spawnShell slave
        installHandler sigCHLD (CatchInfo $ sigchld pid) Nothing
        setTerminalSize master 54 64 120 120
        X.clearWindow d w
    uiLoop

printCmd :: Command -> IO ()
printCmd (Output c) = putChar c
printCmd c = print c

uiLoop :: Ptui ()
uiLoop = do
    c <- use channel
    liftIO $ atomically (readTQueue c) >>= printCmd
    drawGrid
    uiLoop

ui :: X.Display -> TQueue Command -> IO ()
ui d c = forever $ do
    X.sync d True
    X.allocaXEvent $ \e -> do
        X.nextEvent d e
        ev <- XE.getEvent e
        atomically $ writeTQueue c $ X11Event $ XE.eventName ev

pt :: Fd -> TQueue Command -> IO ()
pt f c = input f >>= pure . runFSM >>= mapM_ (atomically . writeTQueue c)
    where input fd = unbuffer fd >>= B.hGetContents
          unbuffer fd = do
              h <- fdToHandle fd
              IO.hSetBinaryMode h True
              IO.hSetBuffering h $ IO.BlockBuffering Nothing
              pure h

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
