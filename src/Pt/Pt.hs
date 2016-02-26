module Pt.Pt where

import Ptui.Types
import Ptui.Config (term)
import Pt.StateMachine
import Pt.Ioctl (setControllingTerminal,setTerminalSize)

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import System.Exit
import System.Posix.User
import System.Posix.Signals as Signals
import System.Posix.Process
import System.Posix.Process.Internals
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd,ProcessID)
import System.Environment
import System.Posix.IO.ByteString
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO as IO

input :: Fd -> IO B.ByteString
input fd = unbuffer fd >>= B.hGetContents

unbuffer :: Fd -> IO IO.Handle
unbuffer fd = do
    h <- fdToHandle fd
    IO.hSetBinaryMode h True
    IO.hSetBuffering h $ IO.BlockBuffering Nothing
    pure h

pt :: IO ()
pt = do
    (master,slave) <- openPseudoTerminal
    forkIO $ input master >>= pure . runFSM >>= mapM_ printCmd >> print "done"
    pid <- spawnShell slave
    installHandler sigCHLD (CatchInfo $ sigchld pid) Nothing
    setTerminalSize master 54 64 120 120
    where printCmd (Output c) = putChar c
          printCmd c = print c

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
