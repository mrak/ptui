module Ptui.Ptui (ptui) where

import Ptui.Args
import Ptui.Settings
import Ptui.Types
import Ptui.Xutils
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State (gets, runStateT)
import qualified Graphics.X11.Xft as Xft
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import System.Exit (exitSuccess)
import System.Time (toCalendarTime, getClockTime, calendarTimeToString)
import qualified Data.Ini as I (readIniFile)

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
    xftDraw <- Xft.xftDrawCreate d w (X.defaultVisual d sn) (X.defaultColormap d sn)
    xftFont <- Xft.xftFontOpen d (X.defaultScreenOfDisplay d) (fontName settings)
    fh <- Xft.xftfont_height xftFont
    fw <- Xft.xftfont_max_advance_width xftFont
    fd <- Xft.xftfont_descent xftFont
    pure PtuiState { cursorPosition = (0,0)
                   , window = w
                   , display = d
                   , screenNumber = sn
                   , screen = X.defaultScreenOfDisplay d
                   , font = xftFont
                   , fontHeight = fh
                   , fontWidth = fw
                   , fontDescent = fd
                   , draw = xftDraw
                   }

date :: IO String
date = do
    t <- toCalendarTime =<< getClockTime
    return $ calendarTimeToString t

loop :: Ptui ()
loop = do
    d <- gets display
    w <- gets window
    fg <- asks colorbg
    bg <- asks colorfg
    liftIO $ X.clearWindow d w
    drawGlyph 0 0 fg bg "A"
    drawGlyph 1 1 fg bg "|"
    drawGlyph 2 2 fg bg "g"
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    loop
