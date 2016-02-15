module Ptui.Pt where

import System.Process
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd)
import System.Environment (getEnvironment, lookupEnv)
import System.Posix.IO.ByteString (fdToHandle)
import Data.Array.IArray (Array)
import Data.Maybe (fromMaybe)

data PtuiCell = PtuiCell { glyph :: String
                         , fg :: String
                         , bg :: String
                         , wide :: Bool
                         }

type PtuiGrid = Array Int (Array Int (Maybe (PtuiCell)))

spawnShell :: IO Fd
spawnShell = fromMaybe "/bin/sh" <$> lookupEnv "SHELL" >>= flip spawnCmd []

spawnCmd :: String -> [String] -> IO Fd
spawnCmd cmd args = do
    (master,slave) <- openPseudoTerminal
    handle <- fdToHandle slave
    environment <- getEnvironment
    (pin,pout,perr,ph) <- createProcess $ CreateProcess {
        cmdspec = RawCommand cmd args,
        cwd = Nothing,
        env = Just $ ("TERM","ptui-256color"):environment,
        std_in = UseHandle handle,
        std_out = UseHandle handle,
        std_err = UseHandle handle,
        close_fds = False,
        create_group = True,
        delegate_ctlc = False
    }
    pure master
