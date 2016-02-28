module Ptui.Ptui (runPtui, nextCommand) where

import Ptui.Args
import Ptui.Config (fromConfig)
import Ptui.Types
import Ui.Xft
import Ui.Xutils
import Ui.ColorCache

import Lens.Simple
import Control.Monad.State (runStateT)
import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display, ScreenNumber, Screen)
import qualified Graphics.X11.Xlib as X
import Data.Array.IArray (assocs,Array, array)
import Data.Map.Strict (Map)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)
import System.Posix.Process (getProcessID)
import Control.Monad.Trans (liftIO)

runPtui :: Ptui p -> Args -> IO (p, PtuiState)
runPtui p a = do
    settings <- fromConfig (config a)
    state <- initState settings
    runStateT (run p) state

nextCommand :: Ptui Command
nextCommand = use channel >>= liftIO . atomically . readTQueue

initState :: PtuiConfig -> IO PtuiState
initState settings = do
    chan <- atomically newTQueue
    x <- initX settings
    ft <- fetchFont x (settings^.cfont)
    pid <- getProcessID
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry (x^.display) (x^.window)
    let cols = quot (fromIntegral ww - (2 * fromIntegral wb)) (ft^.width)
    let rows = quot (fromIntegral wh - (2 * fromIntegral wb)) (ft^.height)
    let rowCells = array (0, cols) [(i,Just PtuiCell {_glyph="X",_fg="red",_bg="white",_wide=False})|i<-[0..cols]]
    let g = array (0, rows) [(i,rowCells)|i<-[0..rows]]
    pure PtuiState { _cursorPosition = (0,0)
                   , _x11 = x
                   , _colors = settings^.ccolors
                   , _font = ft
                   , _grid = g
                   , _channel = chan
                   , _childPid = pid
                   }

drawGrid :: Ptui ()
drawGrid = do
    g <- use grid
    mapM_ drawRow $ assocs g
    where
        drawRow :: (Int, Array Int (Maybe PtuiCell)) -> Ptui ()
        drawRow (y,a) = mapM_ (drawChar y) $ assocs a
        drawChar _ (_,Nothing) = pure ()
        drawChar y (x,Just g) = drawGlyph x y (g^.fg) (g^.bg) (g^.glyph)

drawGlyph :: Int -> Int -> String -> String -> String -> Ptui ()
drawGlyph x y' f b s = do
    let y = y' + 1
    xftFont <- use $ font.face
    htext <- use $ font.height
    wtext <- use $ font.width
    descent <- use $ font.descent
    dpy <- use $ x11.display
    win <- use $ x11.window
    sn <- use $ x11.screenNumber
    liftIO $ withDrawingColors dpy win f b $ \draw f' b' -> do
                drawXftRect draw b' (x * wtext) (y * htext - htext) wtext htext
                drawXftString draw f' xftFont (x * wtext) (y * htext - descent) s
