{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Data.Ini
import Data.Text

data Settings = Settings
    { colorbg :: String
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

fromINI :: Ini -> Settings
fromINI ini = defaultSettings
    { colorbg = lookupIni ini "colors" "background" colorbg
    , colorfg = lookupIni ini "colors" "foreground" colorfg
    }

lookupIni :: Ini -> Text -> Text -> (Settings -> String) -> String
lookupIni ini section key f = fromEither (f defaultSettings) (lookupValue section key ini)

fromEither :: String -> Either String Text -> String
fromEither d e = case e of
                      Left _ -> d
                      Right b -> unpack b

defaultSettings :: Settings
defaultSettings = Settings { colorbg = "#262626"
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
                           , color15 = "#F5F5F5"
                           }
