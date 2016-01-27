module Main where

import Args
import Ptui

main :: IO ()
main = getArgs >>= ptui
