module Ptui.Ptui (ptui) where

import Ptui.Args
import Ptui.Settings
import Ptui.Types
import Ptui.Xutils
import Ptui.Xft
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State (gets, runStateT)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import System.Exit (exitSuccess)
import qualified Data.Ini as I (readIniFile)
import Data.Array.IArray (array)
import Data.Map.Strict (Map, fromList)

ptui :: Args -> IO ()
ptui a = runPtui loop a >> exitSuccess

runPtui :: Ptui p -> Args -> IO (p, PtuiState)
runPtui p a = do
    settings <- fromConfig (config a)
    state <- initState settings
    runStateT (runReaderT (run p) settings) state

initState :: PtuiSettings -> IO PtuiState
initState settings = do
    (d, w) <- initX settings
    let sn = X.defaultScreen d
    xftFont <- openAXftFont d (X.defaultScreenOfDisplay d) (fontName settings)
    fh <- xft_height xftFont
    fw <- xft_max_advance_width xftFont
    fd <- xft_descent xftFont
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry d w
    let cols = quot (fromIntegral ww - (2 * fromIntegral wb)) fw
    let rows = quot (fromIntegral wh - (2 * fromIntegral wb)) fh
    let rowCells = array (0, cols) [(i,Just PtuiCell {glyph="X",fg="red",bg="white",wide=False})|i<-[0..cols]]
    let g = array (0, rows) [(i,rowCells)|i<-[0..rows]]
    pure PtuiState { cursorPosition = (0,0)
                   , window = w
                   , display = d
                   , screenNumber = sn
                   , screen = X.defaultScreenOfDisplay d
                   , font = xftFont
                   , fontHeight = fh
                   , fontWidth = fw
                   , fontDescent = fd
                   , grid = g
                   }

loop :: Ptui ()
loop = do
    d <- gets display
    w <- gets window
    fg <- asks colorbg
    bg <- asks colorfg
    liftIO $ X.clearWindow d w
    drawGrid
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    loop
