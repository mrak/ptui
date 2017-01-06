module Pt.Spawn where

import Pt.Ioctl (setControllingTerminal)
import Ptui.Config (term)

import System.Posix.IO.ByteString (closeFd,dupTo,stdInput,stdOutput,stdError)
import System.Posix.User (userName,getUserEntryForID,getRealUserID)
import Data.Maybe (fromMaybe)
import System.Posix.Process (forkProcess,createSession,executeFile)
import System.Posix.Signals ( Handler(Default)
                            , installHandler
                            , sigCHLD
                            , sigHUP
                            , sigINT
                            , sigQUIT
                            , sigTERM
                            , sigALRM
                            )
import System.Posix.Types (Fd,ProcessID)
import System.Environment (lookupEnv,setEnv,unsetEnv)

shell :: Fd -> IO ProcessID
shell fd = do
    sh <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
    cmd sh [] fd

cmd :: String -> [String] -> Fd -> IO ProcessID
cmd cmd args fd = forkProcess $ do
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

    installHandler sigCHLD Default Nothing
    installHandler sigHUP Default Nothing
    installHandler sigINT Default Nothing
    installHandler sigQUIT Default Nothing
    installHandler sigTERM Default Nothing
    installHandler sigALRM Default Nothing

    executeFile cmd True args Nothing
    pure ()
