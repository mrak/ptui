{-# LANGUAGE OverloadedStrings #-}
module Ptui.Settings (PtuiSettings(..)
                     , fromINI
                     , defaultSettings) where

import Ptui.Types
import Data.Ini
import Data.Text

fromINI :: Ini -> PtuiSettings
fromINI ini = defaultSettings { cursorc = lookupIni ini "colors" "cursor" cursorc
                              , colorbg = lookupIni ini "colors" "background" colorbg
                              , colorfg = lookupIni ini "colors" "foreground" colorfg
                              , color00 = lookupIni ini "colors" "black" color00
                              , color01 = lookupIni ini "colors" "red" color01
                              , color02 = lookupIni ini "colors" "green" color02
                              , color03 = lookupIni ini "colors" "yellow" color03
                              , color04 = lookupIni ini "colors" "blue" color04
                              , color05 = lookupIni ini "colors" "magenta" color05
                              , color06 = lookupIni ini "colors" "cyan" color06
                              , color07 = lookupIni ini "colors" "white" color07
                              , color08 = lookupIni ini "colors" "bright black" color08
                              , color09 = lookupIni ini "colors" "bright red" color09
                              , color10 = lookupIni ini "colors" "bright green" color10
                              , color11 = lookupIni ini "colors" "bright yellow" color11
                              , color12 = lookupIni ini "colors" "bright blue" color12
                              , color13 = lookupIni ini "colors" "bright magenta" color13
                              , color14 = lookupIni ini "colors" "bright cyan" color14
                              , color15 = lookupIni ini "colors" "bright white" color15
                              }

lookupIni :: Ini -> Text -> Text -> (PtuiSettings -> String) -> String
lookupIni ini section key f = fromEither (f defaultSettings) (lookupValue section key ini)

fromEither :: String -> Either String Text -> String
fromEither d e = case e of
                      Left _ -> d
                      Right b -> unpack b
