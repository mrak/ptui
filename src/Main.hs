module Main where

import Ptui.Args
import Ptui.Ptui

main :: IO ()
main = getArgs >>= ptui
