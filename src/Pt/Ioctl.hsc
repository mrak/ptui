{-# LANGUAGE ForeignFunctionInterface #-}

module Pt.Ioctl
    ( setControllingTerminal
    , setTerminalSize
    ) where

import System.Posix.Types
import Foreign
import Foreign.C.Error
import Foreign.C.Types

#include <sys/ioctl.h>
#include <unistd.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

foreign import ccall "sys/ioctl.h ioctl" tiocsctty :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/ioctl.h ioctl" tiocswinsz :: CInt -> CInt -> Ptr Winsize -> IO CInt

data Winsize = Winsize { row :: CUShort
                       , col :: CUShort
                       , xpixel :: CUShort
                       , ypixel :: CUShort
                       }

instance Storable Winsize where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize)
  peek ptr = do
    r <- (#peek struct winsize, ws_row) ptr
    c <- (#peek struct winsize, ws_col) ptr
    x <- (#peek struct winsize, ws_xpixel) ptr
    y <- (#peek struct winsize, ws_ypixel) ptr
    pure $ Winsize r c x y
  poke ptr (Winsize r c x y) = do
    (#poke struct winsize, ws_row) ptr r
    (#poke struct winsize, ws_col) ptr c
    (#poke struct winsize, ws_xpixel) ptr x
    (#poke struct winsize, ws_ypixel) ptr y

setTerminalSize :: Fd -> Int -> Int -> Int -> Int -> IO ()
setTerminalSize (Fd fd) r c x y =
    with (Winsize (fromIntegral r) (fromIntegral c) (fromIntegral x) (fromIntegral y)) $ \ws -> do
        throwErrnoIfMinus1 "ioctl" $ tiocswinsz fd (#const TIOCSWINSZ) ws
        pure ()

setControllingTerminal :: Fd -> IO ()
setControllingTerminal (Fd fd) = do
    throwErrnoIfMinus1 "ioctl" $ tiocsctty fd (#const TIOCSCTTY) 1
    pure ()
