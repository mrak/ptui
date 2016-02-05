{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ptui.Ptui (Ptui, runPtui) where

import Ptui.Args
import Ptui.Settings
import Ptui.State
import Ptui.Vt
import Ptui.Xft
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.State (StateT, MonadState, runStateT)
import Control.Monad.IO.Class (MonadIO)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import System.Exit (exitSuccess)
import qualified Data.Ini as I (readIniFile)
import Data.Array.IArray (array)
import Data.Map.Strict (Map, fromList)

newtype Ptui a = Ptui { run :: ReaderT PtuiSettings (StateT PtuiState IO) a
                      } deriving (Functor, Applicative, Monad, MonadIO, MonadReader PtuiSettings, MonadState PtuiState)

runPtui :: Ptui p -> Args -> IO (p, PtuiState)
runPtui p a = do
    settings <- fromConfig (config a)
    state <- initState settings
    runStateT (runReaderT (run p) settings) state

