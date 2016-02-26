{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ptui.Types where

import Ui.Xft (AXftFont)

import Data.Array.IArray (Array)
import Control.Monad.State (StateT,MonadState)
import Control.Monad.IO.Class (MonadIO)
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import Control.Concurrent.STM.TChan (TChan)

data Args = Args
    { config :: FilePath
    }

data CLIArgs = CLIArgs
    { cliConfig :: Maybe FilePath
    }

newtype Ptui a = Ptui { run :: StateT PtuiState IO a
                      } deriving (Functor, Applicative, Monad, MonadIO, MonadState PtuiState)

data PtuiX11 = PtuiX11
             { display :: Display
             , window :: Window
             , screen :: Screen
             , screenNumber :: ScreenNumber
             }

data PtuiState = PtuiState { cursorPosition :: (Int, Int)
                           , x11 :: PtuiX11
                           , colors :: PtuiColors
                           , font :: AXftFont
                           , fontHeight :: Int
                           , fontWidth :: Int
                           , fontDescent :: Int
                           , grid :: PtuiGrid
                           , channel :: TChan Command
                           }



data PtuiColors = PtuiColors { foreground :: String
                             , background :: String
                             , cursor :: String
                             , table :: Array Int String
                             }

data PtuiWindow = PtuiWindow
                  { title :: String
                  , clazz :: String
                  }

data PtuiConfig = PtuiConfig
                { ccolors :: PtuiColors
                , cfont :: String
                , cwindow :: PtuiWindow
                }


data PtuiCell = PtuiCell { glyph :: String
                         , fg :: String
                         , bg :: String
                         , wide :: Bool
                         } deriving Show

type PtuiGrid = Array Int (Array Int (Maybe PtuiCell))

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
             deriving Show

