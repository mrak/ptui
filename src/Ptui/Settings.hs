{-# LANGUAGE OverloadedStrings #-}
module Ptui.Settings (PtuiSettings(..)
                     , fromConfig
                     , defaultSettings) where

import Ptui.Types
import Data.Ini (readIniFile, Ini, lookupValue)
import Data.Text (unpack, Text)
import Data.Either (either)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)

fromINI :: Ini -> PtuiSettings
fromINI ini = PtuiSettings { cursorc = lookupString ini "colors" "cursor" cursorc
                           , colorbg = lookupString ini "colors" "background" colorbg
                           , colorfg = lookupString ini "colors" "foreground" colorfg
                           , color00 = lookupString ini "colors" "black" color00
                           , color01 = lookupString ini "colors" "red" color01
                           , color02 = lookupString ini "colors" "green" color02
                           , color03 = lookupString ini "colors" "yellow" color03
                           , color04 = lookupString ini "colors" "blue" color04
                           , color05 = lookupString ini "colors" "magenta" color05
                           , color06 = lookupString ini "colors" "cyan" color06
                           , color07 = lookupString ini "colors" "white" color07
                           , color08 = lookupString ini "colors" "bright black" color08
                           , color09 = lookupString ini "colors" "bright red" color09
                           , color10 = lookupString ini "colors" "bright green" color10
                           , color11 = lookupString ini "colors" "bright yellow" color11
                           , color12 = lookupString ini "colors" "bright blue" color12
                           , color13 = lookupString ini "colors" "bright magenta" color13
                           , color14 = lookupString ini "colors" "bright cyan" color14
                           , color15 = lookupString ini "colors" "bright white" color15
                           , fontName = lookupString ini "font" "name" fontName
                           , fontSize = lookupInt ini "font" "size" fontSize
                           }

lookupInt :: Ini -> Text -> Text -> (PtuiSettings -> Int) -> Int
lookupInt ini section key f = either (const $ f defaultSettings) (read.unpack) (lookupValue section key ini)

lookupString :: Ini -> Text -> Text -> (PtuiSettings -> String) -> String
lookupString ini section key f = either (const $ f defaultSettings) unpack (lookupValue section key ini)

fromConfig :: FilePath -> IO PtuiSettings
fromConfig fp = do
    exists <- doesFileExist fp
    if exists
       then readIniFile fp >>= pure . either (const defaultSettings) fromINI
       else warn ("Configuration file " ++ fp ++ " does not exist. Using default settings.") >> pure defaultSettings

warn :: String -> IO ()
warn = hPutStrLn stderr
