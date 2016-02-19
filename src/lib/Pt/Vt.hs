module Pt.Vt where

import Pt.StateMachine
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO as IO

input :: IO.Handle -> IO B.ByteString
input h = IO.hSetBinaryMode h True >> IO.hSetBuffering h IO.NoBuffering >> B.hGetContents h

go :: IO ()
go = runFSM <$> input IO.stdin >>= mapM_ print
