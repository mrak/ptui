module Pt.Commands where

import Ptui.Types
import Pt.StateMachine
import Control.Concurrent.STM (atomically,TQueue,writeTQueue)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Posix.Types (Fd)
import qualified System.IO as IO
import System.Posix.IO.ByteString (fdToHandle)

stream :: Fd -> TQueue Command -> IO ()
stream f c = input f >>= pure . runFSM >>= mapM_ (atomically . writeTQueue c)
    where input fd = unbuffer fd >>= B.hGetContents
          unbuffer fd = do
              h <- fdToHandle fd
              IO.hSetBinaryMode h True
              IO.hSetBuffering h $ IO.BlockBuffering Nothing
              pure h
