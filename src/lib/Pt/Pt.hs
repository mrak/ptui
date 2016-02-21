module Pt.Pt where

import Ptui.Config (term)
import Pt.StateMachine

import System.Process
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

input :: IO.Handle -> IO B.ByteString
input h = IO.hSetBinaryMode h True >> IO.hSetBuffering h IO.NoBuffering >> B.hGetContents h

pt :: IO ()
pt = spawnShell >>= fdToHandle >>= input >>= pure . runFSM >>= mapM_ print

spawnShell :: IO Fd
spawnShell = fromMaybe "/bin/sh" <$> lookupEnv "SHELL" >>= flip spawnCmd []

spawnCmd :: String -> [String] -> IO Fd
spawnCmd cmd args = do
    (master,slave) <- openPseudoTerminal
    handle <- fdToHandle slave
    environment <- getEnvironment
    (pin,pout,perr,ph) <- createProcess CreateProcess {
        cmdspec = RawCommand cmd args,
        cwd = Nothing,
        env = Just $ ("TERM",term):environment,
        std_in = UseHandle handle,
        std_out = UseHandle handle,
        std_err = UseHandle handle,
        close_fds = True,
        create_group = True,
        delegate_ctlc = False
    }
    pure master
