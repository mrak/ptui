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
    xftDraw <- Xft.xftDrawCreate d w (X.defaultVisual d sn) (X.defaultColormap d sn)
    xftFont <- Xft.xftFontOpen d (X.defaultScreenOfDisplay d) (fontName settings)
    fh <- Xft.xftfont_height xftFont
    fw <- Xft.xftfont_max_advance_width xftFont
    fd <- Xft.xftfont_descent xftFont
    (_, wx, wy, ww, wh, wb, _) <- X.getGeometry d w
    ccache <- initColorCache d settings
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
                   , draw = xftDraw
                   , grid = g
                   , colorCache = ccache
                   }

initColorCache :: X.Display -> PtuiSettings -> IO (Map String Xft.XftColor)
initColorCache d s = do
    x00 <- xftcolor d $ color00 s
    x01 <- xftcolor d $ color01 s
    x02 <- xftcolor d $ color02 s
    x03 <- xftcolor d $ color03 s
    x04 <- xftcolor d $ color04 s
    x05 <- xftcolor d $ color05 s
    x06 <- xftcolor d $ color06 s
    x07 <- xftcolor d $ color07 s
    x08 <- xftcolor d $ color08 s
    x09 <- xftcolor d $ color09 s
    x10 <- xftcolor d $ color10 s
    x11 <- xftcolor d $ color11 s
    x12 <- xftcolor d $ color12 s
    x13 <- xftcolor d $ color13 s
    x14 <- xftcolor d $ color14 s
    x15 <- xftcolor d $ color15 s
    pure $ fromList [ (color00 s, x00)
                    , (color01 s, x01)
                    , (color01 s, x02)
                    , (color01 s, x03)
                    , (color01 s, x04)
                    , (color01 s, x05)
                    , (color01 s, x06)
                    , (color01 s, x07)
                    , (color01 s, x08)
                    , (color01 s, x09)
                    , (color01 s, x10)
                    , (color01 s, x11)
                    , (color01 s, x12)
                    , (color01 s, x13)
                    , (color01 s, x14)
                    , (color01 s, x15)
                    ]
    where
        sn = X.defaultScreen d
        xftcolor d s = Xft.withXftColorName d (X.defaultVisual d sn) (X.defaultColormap d sn) s pure


loop :: Ptui ()
loop = do
    d <- gets display
    w <- gets window
    fg <- asks colorbg
    bg <- asks colorfg
    liftIO $ X.clearWindow d w
    drawGrid
    {-drawGlyph 0 0 fg bg "A"-}
    {-drawGlyph 1 1 fg bg "|"-}
    {-drawGlyph 2 2 fg bg "g"-}
    liftIO $ do
        X.sync d True
        X.allocaXEvent $ \e -> do
            X.nextEvent d e
            ev <- XE.getEvent e
            putStrLn $ XE.eventName ev
    loop
