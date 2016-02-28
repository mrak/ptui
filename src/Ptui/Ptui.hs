module Ptui.Ptui where

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
import Graphics.X11.Xlib.Misc (lockDisplay,unlockDisplay)
import qualified Graphics.X11.Xlib as X
import Data.Array.IArray (assocs,Array,array,bounds)
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
    let g = array (0, rows * cols) [(i,PtuiCell {_glyph='X',_fg="red",_bg="white",_wide=False,_dirty=True})|i<-[0..cols]]
    pure PtuiState { _cursorPosition = (0,0)
                   , _x11 = x
                   , _colors = settings^.ccolors
                   , _font = ft
                   , _grid = PtuiGrid rows cols g
                   , _channel = chan
                   , _childPid = pid
                   }

drawGrid :: Ptui ()
drawGrid = do
    rs <- use $ grid.rows
    cs <- use $ grid.cols
    g <- use $ grid.cells
    newCells <- mapM drawCell (assocs g)
    liftIO $ print "here"
    grid.cells .= array (0,rs*cs) newCells

drawCell :: (Int,PtuiCell) -> Ptui (Int,PtuiCell)
drawCell t@(i,cell) | not (cell^.dirty) = pure t
                    | otherwise = do
    liftIO $ print i
    rs <- use $ grid.rows
    cs <- use $ grid.cols
    let x = mod i cs
    let y = mod i rs + 1
    let f = cell^.fg
    let b = cell^.bg
    let c = cell^.glyph
    xftFont <- use $ font.face
    htext <- use $ font.height
    wtext <- use $ font.width
    descent <- use $ font.descent
    dpy <- use $ x11.display
    win <- use $ x11.window
    sn <- use $ x11.screenNumber
    liftIO $ do
        lockDisplay dpy
        withDrawingColors dpy win f b $ \draw f' b' -> do
            drawXftRect draw b' (x * wtext) (y * htext - htext) wtext htext
            drawXftString draw f' xftFont (x * wtext) (y * htext - descent) [c]
        unlockDisplay dpy
    pure (i,cell { _dirty = False })
