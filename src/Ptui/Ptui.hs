module Ptui.Ptui (ptui) where

import Ptui.Args
import Ptui.Settings
import Ptui.Events
import Ptui.Types
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State (gets, runStateT)
import Control.Concurrent
import Control.Concurrent.STM (newTQueueIO, tryReadTQueue, atomically)
import qualified Graphics.X11.Xft as Xft
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xrender as XR
import qualified Graphics.X11.Xlib.Extras as XE
import System.Exit (exitSuccess)
import System.Time
import Data.Bits ((.|.))
import qualified Data.Ini as I (readIniFile)

ptui :: Args -> IO ()
ptui a = runPtui loop a >> exitSuccess

runPtui :: Ptui p -> Args -> IO (p, PtuiState)
runPtui p a = do
    settings <- readSettings (config a)
    state <- initState settings
    runStateT (runReaderT (run p) settings) state

initState :: PtuiSettings -> IO PtuiState
initState settings = do
    d <- X.openDisplay ""
    let s = X.defaultScreen d
        border = X.blackPixel d s
    background <- initColor d (colorbg settings)
    rootw <- X.rootWindow d s
    w <- X.createSimpleWindow d rootw 0 0 100 100 0 border background
    X.setTextProperty d w "ptui" X.wM_NAME
    X.mapWindow d w
    X.selectInput d w (X.exposureMask .|. X.buttonPressMask)
    q <- newTQueueIO
    setupEventQueue q w
    pure PtuiState { cursorPosition = (0,0)
                   , window = w
                   , display = d
                   , screenNumber = s
                   , screen = X.screenOfDisplay d s
                   , queue = q
                   }

initColor :: X.Display -> String -> IO X.Pixel
initColor display color = do
  let colormap = X.defaultColormap display (X.defaultScreen display)
  (apros,real) <- X.allocNamedColor display colormap color
  pure $ X.color_pixel apros

readSettings :: FilePath -> IO PtuiSettings
readSettings fp = do
    exists <- doesFileExist fp
    if exists
       then I.readIniFile fp >>= pure . either (const defaultSettings) fromINI
       else warn ("Configuration file " ++ fp ++ " does not exist. Using default settings.") >> pure defaultSettings

warn :: String -> IO ()
warn = hPutStrLn stderr

drawInWin :: String -> Ptui ()
drawInWin str = do
    fn <- asks fontName
    fg <- asks colorfg
    win <- gets window
    dpy <- gets display
    scr <- gets screen
    sn <- gets screenNumber
    liftIO $ do
        bgcolor <- initColor dpy "green"
        fgcolor <- initColor dpy "blue"
        xftFont <- Xft.xftFontOpen dpy scr fn
        extents <- Xft.xftTextExtents dpy xftFont "0"
        let htext = XR.xglyphinfo_height extents - XR.xglyphinfo_y extents
        let wtext = XR.xglyphinfo_width extents - XR.xglyphinfo_x extents
        xftDraw <- Xft.xftDrawCreate dpy win (X.defaultVisual dpy sn) (X.defaultColormap dpy sn)
        X.clearWindow dpy win
        Xft.withXftColorName dpy (X.defaultVisual dpy sn) (X.defaultColormap dpy sn) fg $ \c -> Xft.xftDrawString xftDraw c xftFont 100 100 "Hello, World!"

date :: IO String
date = do
    t <- toCalendarTime =<< getClockTime
    return $ calendarTimeToString t

loop :: Ptui ()
loop = do
    d <- gets display
    w <- gets window
    drawInWin =<< liftIO date
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    loop
