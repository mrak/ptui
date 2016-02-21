module Pt.Pt where

import Ptui.Config (term)
import Pt.StateMachine

import Control.Concurrent.MVar (newEmptyMVar,takeMVar,putMVar)
import Control.Concurrent (forkIO)
import System.Process
import System.Posix.Process (forkProcess,createSession)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd)
import System.Environment (getEnvironment, lookupEnv)
import System.Posix.IO.ByteString (fdToHandle)
import Data.Array.IArray (Array)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO as IO

data PtuiCell = PtuiCell { glyph :: String
                         , fg :: String
                         , bg :: String
                         , wide :: Bool
                         } deriving Show

type PtuiGrid = Array Int (Array Int (Maybe PtuiCell))

input :: Fd -> IO B.ByteString
input fd = unbuffer fd >>= B.hGetContents

unbuffer :: Fd -> IO IO.Handle
unbuffer fd = do
    h <- fdToHandle fd
    IO.hSetBinaryMode h True
    IO.hSetBuffering h IO.NoBuffering
    pure h

pt :: IO ()
pt = do
    (master,slave) <- openPseudoTerminal
    mvar <- newEmptyMVar
    forkIO $ input master >>= pure . runFSM >>= mapM_ printCmd >> putMVar mvar ()
    unbuffer slave >>= spawnCmd "/bin/fish" ["-c","isatty"]
    {-unbuffer slave >>= spawnShell-}
    takeMVar mvar
    where printCmd (Output c) = putChar c
          printCmd c = print c

spawnShell :: IO.Handle -> IO ()
spawnShell h = fromMaybe "/bin/sh" <$> lookupEnv "SHELL" >>= \s -> spawnCmd s [] h

spawnCmd :: String -> [String] -> IO.Handle -> IO ()
spawnCmd cmd args handle = do
    environment <- getEnvironment
    home <- lookupEnv "HOME"
    createProcess CreateProcess
        { cmdspec = RawCommand cmd args
        , cwd = home
        , env = Just $ ("TERM",term):environment
        , std_in = UseHandle handle
        , std_out = UseHandle handle
        , std_err = UseHandle handle
        , close_fds = True
        , create_group = True
        , new_session = True
        , delegate_ctlc = True
        , child_group = Nothing
        , child_user = Nothing
        -- windows, ignored
        , detach_console = False
        , create_new_console = False
        }
    pure ()
