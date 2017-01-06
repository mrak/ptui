module Main where

import Ptui.Ptui (runPtui,ptui)
import Ptui.Args (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = getArgs >>= runPtui ptui >> exitSuccess

