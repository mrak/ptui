{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ptui.Types where

import Ui.Xft (AXftFont)

import Lens.Simple
import Data.Array.IArray (Array)
import Control.Monad.State (StateT,MonadState)
import Control.Monad.IO.Class (MonadIO)
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import Control.Concurrent.STM (TQueue)

data Args = Args
    { config :: FilePath
    }

data CLIArgs = CLIArgs
    { cliConfig :: Maybe FilePath
    }

data PtuiCell = PtuiCell { _glyph :: String
                         , _fg :: String
                         , _bg :: String
                         , _wide :: Bool
                         } deriving Show

type PtuiGrid = Array Int (Array Int (Maybe PtuiCell))

data PtuiColors = PtuiColors { _foreground :: String
                             , _background :: String
                             , _cursor :: String
                             , _table :: Array Int String
                             }

data PtuiX11 = PtuiX11
             { _display :: Display
             , _window :: Window
             , _screen :: Screen
             , _screenNumber :: ScreenNumber
             }

data PtuiWindow = PtuiWindow
                  { _title :: String
                  , _clazz :: String
                  }

data PtuiConfig = PtuiConfig
                { _ccolors :: PtuiColors
                , _cfont :: String
                , _cwindow :: PtuiWindow
                }


newtype Ptui a = Ptui { run :: StateT PtuiState IO a
                      } deriving (Functor, Applicative, Monad, MonadIO, MonadState PtuiState)



data CharacterSetSlot = G0
                      | G1
                      | G2
                      | G3
                      deriving Show

data CharacterSet = Special
                  | UK
                  | USASCII
                  deriving Show

data Color = Color256 Int
           | Truecolor Int Int Int
           | Default
           deriving (Show,Eq)

data SGRAttr = Reset
             | Bold Bool
             | Faint
             | Blink Bool
             | Underscore Bool
             | Italic Bool
             | Reverse Bool
             | Foreground Color
             | Background Color
             | Invisible Bool
             | Strikethrough Bool
             | DoubleUnderline
             deriving (Show,Eq)

data Command = Noop
             | Output Char
             | BEL
             | BS
             | HT
             | LF
             | VT
             | FF
             | CR
             | ENQ
             | IND
             | NEL
             | HTS
             | RI
             | SS2 Char
             | SS3 Char
             | SGR [SGRAttr]
             | CUU Int
             | CUD Int
             | CUF Int
             | CUB Int
             | CNL Int
             | CPL Int
             | CHA Int
             | CHT Int
             | CUP Int Int
             | SetCharset CharacterSetSlot CharacterSet
             | SetIconTitle String
             | X11Event String
             deriving Show

data PtuiState = PtuiState { _cursorPosition :: (Int, Int)
                           , _x11 :: PtuiX11
                           , _colors :: PtuiColors
                           , _font :: AXftFont
                           , _fontHeight :: Int
                           , _fontWidth :: Int
                           , _fontDescent :: Int
                           , _grid :: PtuiGrid
                           , _channel :: TQueue Command
                           }

makeLenses ''PtuiCell
makeLenses ''PtuiState
makeLenses ''PtuiColors
makeLenses ''PtuiWindow
makeLenses ''PtuiConfig
makeLenses ''PtuiX11
