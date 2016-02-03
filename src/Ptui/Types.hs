{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ptui.Types where

import Ptui.Events
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import Control.Concurrent.STM (TQueue)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.IO.Class (MonadIO)

newtype Ptui a = Ptui { run :: ReaderT PtuiSettings (StateT PtuiState IO) a
                      } deriving (Functor, Applicative, Monad, MonadIO, MonadReader PtuiSettings, MonadState PtuiState)

data PtuiState = PtuiState { cursorPosition :: (Int, Int)
                           , window :: Window
                           , display :: Display
                           , screen :: Screen
                           , screenNumber :: ScreenNumber
                           , queue :: TQueue Event
                           }

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
                  , fontName :: String
                  , fontSize :: Int
                  } deriving (Show)

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
                               , fontName = "monospace"
                               , fontSize = 10
                               }

data Args = Args
    { config :: FilePath
    }

data CLIArgs = CLIArgs
    { cliConfig :: Maybe FilePath
    }

