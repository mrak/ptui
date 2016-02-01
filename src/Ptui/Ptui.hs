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
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit (exitSuccess)
import System.Time
import Data.Bits ((.|.))
import qualified Data.Ini as I (readIniFile)

ptui :: Args -> IO ()
ptui a = do
    d <- openDisplay ""
    let s = defaultScreen d
        border = blackPixel d s
    background <- initColor d "#262626"
    rootw <- rootWindow d s
    w <- createSimpleWindow d rootw 0 0 100 100 1 border background
    setTextProperty d w "ptui" wM_NAME
    mapWindow d w
    selectInput d w (exposureMask .|. buttonPressMask)
    q <- newTQueueIO
    settings <- readSettings (config a)
    let state = PtuiState { cursorPosition = (0,0)
                          , window = w
                          , display = d
                          , screenNumber = s
                          , queue = q
                          }
    setupEventQueue q w
    runStateT (runReaderT (run loop) settings) state
    exitSuccess

initColor :: Display -> String -> IO Pixel
initColor display color = do
  let colormap = defaultColormap display (defaultScreen display)
  (apros,real) <- allocNamedColor display colormap color
  return $ color_pixel apros

readSettings :: FilePath -> IO PtuiSettings
readSettings fp = do
    exists <- doesFileExist fp
    if exists
       then I.readIniFile fp >>= pure . either (const defaultSettings) fromINI
       else warn ("Configuration file " ++ fp ++ " does not exist. Using default settings.") >> pure defaultSettings

warn :: String -> IO ()
warn = hPutStrLn stderr

drawInWin :: Display -> Window -> String ->IO ()
drawInWin dpy win str = do
    bgcolor <- initColor dpy "green"
    fgcolor <- initColor dpy "blue"
    gc <- createGC dpy win
    fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    p <- createPixmap dpy win 200 100 (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
    setForeground dpy gc bgcolor
    fillRectangle dpy p gc 0 0 200 100
    setForeground dpy gc fgcolor
    fillRectangle dpy p gc 2 2 196 96
    printString dpy p gc fontStruc str
    copyArea dpy p win gc 0 0 200 100 0 0
    freeGC dpy gc
    freeFont dpy fontStruc
    freePixmap dpy p

date :: IO String
date = do
    t <- toCalendarTime =<< getClockTime
    return $ calendarTimeToString t

printString :: Display -> Drawable -> GC -> FontStruct -> String -> IO ()
printString dpy d gc fontst str = do
    let strLen = textWidth fontst str
        (_,asc,_,_) = textExtents fontst str
        valign = (100 + fromIntegral asc) `div` 2
        remWidth = 200 - strLen
        offset = remWidth `div` 2
    fgcolor <- initColor dpy "white"
    bgcolor <- initColor dpy "blue"
    setForeground dpy gc fgcolor
    setBackground dpy gc bgcolor
    drawImageString dpy d gc offset valign str

loop :: Ptui ()
loop = do
    d <- gets display
    w <- gets window
    liftIO $ do
        drawInWin d w =<< date
        sync d True
        allocaXEvent $ \e -> do
            nextEvent d e
            ev <- getEvent e
            putStrLn $ eventName ev
    loop
