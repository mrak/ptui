module Main where

import Ptui.Args
import Ptui.Types
import Ptui.Ptui
import Pt.Pt
import Ui.Ui

import System.Exit (exitSuccess)

main :: IO ()
main = getArgs >>= runPtui pt ui >> exitSuccess

