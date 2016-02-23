{-# LANGUAGE ForeignFunctionInterface #-}

module Pt.Ioctl (setControllingTerminal) where

import System.Posix.Types
import Foreign
import Foreign.C.Error
import Foreign.C.Types

#include <sys/ioctl.h>
#include <unistd.h>

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> CInt -> IO CInt

setControllingTerminal :: Fd -> IO ()
setControllingTerminal (Fd fd) = do
    throwErrnoIfMinus1 "ioctl" $ ioctl fd (#const TIOCSCTTY) 1
    pure ()
