{-# LANGUAGE OverloadedStrings #-}
module Ptui.Settings (PtuiSettings(..)
                     , fromINI
                     , defaultSettings) where

import Data.Ini
import Data.Text

data PtuiSettings = PtuiSettings
                  { cursorc :: String
                  , colorbg :: String
                  , colorfg :: String
                  , color00 :: String
                  , color01 :: String
                  , color02 :: String
                  , color03 :: String
                  , color04 :: String
                  , color05 :: String
                  , color06 :: String
                  , color07 :: String
                  , color08 :: String
                  , color09 :: String
                  , color10 :: String
                  , color11 :: String
                  , color12 :: String
                  , color13 :: String
                  , color14 :: String
                  , color15 :: String
                  } deriving (Show)

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

defaultSettings :: PtuiSettings
defaultSettings = PtuiSettings { cursorc = "#FFFFFF"
                               , colorbg = "#262626"
                               , colorfg = "#D3D7CF"
                               , color00 = "#000000"
                               , color01 = "#AC4142"
                               , color02 = "#4E9A06"
                               , color03 = "#C4A000"
                               , color04 = "#3465A4"
                               , color05 = "#75507B"
                               , color06 = "#06989A"
                               , color07 = "#D3D7CF"
                               , color08 = "#555753"
                               , color09 = "#D24545"
                               , color10 = "#8AE234"
                               , color11 = "#FCE94F"
                               , color12 = "#729FCF"
                               , color13 = "#AD7FA8"
                               , color14 = "#34E2E2"
                               , color15 = "#FFFFFF"
                               }
