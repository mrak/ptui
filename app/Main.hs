module Main where

import Args
import Lib

main :: IO ()
main = getArgs >>= ptui
