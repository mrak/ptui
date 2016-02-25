{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ptui.Types where

import Ui.Xft (AXftFont)

import Data.Array.IArray (Array)
import Control.Monad.State (StateT,MonadState)
import Control.Monad.IO.Class (MonadIO)
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)

data Args = Args
    { config :: FilePath
    }

data CLIArgs = CLIArgs
    { cliConfig :: Maybe FilePath
    }

newtype Ui a = Ui { runUi :: StateT UiState IO a
                  } deriving (Functor, Applicative, Monad, MonadIO, MonadState UiState)

newtype Pt a = Pt { runPt :: StateT PtState IO a
                  } deriving (Functor, Applicative, Monad, MonadIO, MonadState PtState)

data PtState = PtState {}

data UiX11 = UiX11
               { display :: Display
               , window :: Window
               , screen :: Screen
               , screenNumber :: ScreenNumber
               }

data UiState = UiState { cursorPosition :: (Int, Int)
                       , x11 :: UiX11
                       , colors :: PtuiColors
                       , font :: AXftFont
                       , fontHeight :: Int
                       , fontWidth :: Int
                       , fontDescent :: Int
                       , grid :: PtuiGrid
                       }



data PtuiColors = PtuiColors { foreground :: String
                             , background :: String
                             , cursor :: String
                             , table :: Array Int String
                             }

data UiWindow = UiWindow
                  { title :: String
                  , clazz :: String
                  }

data PtuiConfig = PtuiConfig
                  { ccolors :: PtuiColors
                  , cfont :: String
                  , cwindow :: UiWindow
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

