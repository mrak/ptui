module Ptui.Vt where

import Ptui.StateMachine
import qualified Data.ByteString.Lazy.Char8 as B
import qualified System.IO as IO

input :: IO.Handle -> IO B.ByteString
input h = IO.hSetBinaryMode h True >> IO.hSetBuffering h IO.NoBuffering >> B.hGetContents h

main :: IO ()
main = transduce Q0 Noop <$> input IO.stdin >>= mapM_ print

