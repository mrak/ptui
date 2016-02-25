{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Pt.StateMachine where

import Ptui.Types

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (isDigit)
import Data.List (foldl',uncons)
import Data.Maybe (mapMaybe,fromMaybe)
import Text.Read (readMaybe)

data CX = C0 | C1 deriving Show

data Q_ = Q0
          | QSS2
          | QSS3
          | QESC
          | QESCSP
          | QCSI
          | QDCS
          | QOSC
          | QOSC2
          | QSGR0
          | QSGR38
          | QSGR38c
          | QSGR38r
          | QSGR38g
          | QSGR38b
          | QSGR48
          | QSGR48c
          | QSGR48r
          | QSGR48g
          | QSGR48b
          deriving Show

pattern b :> bs <- (B.uncons -> Just (b,bs))
pattern Empty   <- (B.uncons -> Nothing)

runFSM :: B.ByteString -> [Command]
runFSM = transduce Q0 C0 ("","",[])

runFSMC1 :: B.ByteString -> [Command]
runFSMC1 = transduce Q0 C1 ("","",[])

transduce :: Q_ -> CX -> (String,String,[String]) -> B.ByteString -> [Command]
transduce _ _ _ Empty         = []

transduce Q0 cs t ('\x1b':>bs) = transduce QESC cs t bs
transduce Q0 cs t ('\x07':>bs) = BEL : transduce Q0 cs t bs
transduce Q0 cs t ('\x08':>bs) = BS : transduce Q0 cs t bs
transduce Q0 cs t ('\x09':>bs) = HT : transduce Q0 cs t bs
transduce Q0 cs t ('\x0a':>bs) = LF : transduce Q0 cs t bs
transduce Q0 cs t ('\x0b':>bs) = VT : transduce Q0 cs t bs
transduce Q0 cs t ('\x0c':>bs) = FF : transduce Q0 cs t bs
transduce Q0 cs t ('\x0d':>bs) = CR : transduce Q0 cs t bs
transduce Q0 C1 t ('\x84':>bs) = IND : transduce Q0 C1 t bs
transduce Q0 C1 t ('\x85':>bs) = NEL : transduce Q0 C1 t bs
transduce Q0 C1 t ('\x88':>bs) = HTS : transduce Q0 C1 t bs
transduce Q0 C1 t ('\x8d':>bs) = RI : transduce Q0 C1 t bs
transduce Q0 C1 t ('\x8e':>bs) = transduce QSS2 C1 t bs
transduce Q0 C1 t ('\x8f':>bs) = transduce QSS3 C1 t bs
transduce Q0 C1 t ('\x9b':>bs) = transduce QCSI C1 t bs
transduce Q0 cs t bs = maybe [] (\(b,bs') -> Output b : transduce Q0 cs t bs') (U.uncons bs)

transduce QSS2 cs t (b:>bs) = SS2 b : transduce Q0 cs t bs
transduce QSS3 cs t (b:>bs) = SS3 b : transduce Q0 cs t bs

transduce QESC cs t ('D':>bs) = IND : transduce Q0 cs t bs
transduce QESC cs t ('E':>bs) = NEL : transduce Q0 cs t bs
transduce QESC cs t ('H':>bs) = HTS : transduce Q0 cs t bs
transduce QESC cs t ('M':>bs) = RI : transduce Q0 cs t bs
transduce QESC cs t ('N':>bs) = transduce QSS2 cs t bs
transduce QESC cs t ('O':>bs) = transduce QSS3 cs t bs
transduce QESC cs _ ('[':>bs) = transduce QCSI cs ("","",[]) bs
transduce QESC cs _ ('(':>bs) = transduce QDCS cs ("(","",[]) bs
transduce QESC cs _ (']':>bs) = transduce QOSC cs ("","",[]) bs
transduce QESC cs t (' ':>bs) = transduce QESCSP cs t bs

transduce QOSC cs t@(p,x,xs) (';':>bs) = transduce QOSC2 cs (p,"",xs++[x]) bs
transduce QOSC cs t@(p,x,xs) (b:>bs) | isDigit b = transduce QOSC cs (p,x++[b],xs) bs
                                     | otherwise = transduce Q0 cs t bs
transduce QOSC2 cs t@(p,x,xs) ('\a':>bs) = makeOSC (xs++[x]) : transduce Q0 cs t bs
transduce QOSC2 C1 t@(p,x,xs) ('\x9c':>bs) = makeOSC (xs++[x]) : transduce Q0 C1 t bs
transduce QOSC2 cs t@(p,x,xs) bs = maybe [] (\(b,bs') -> transduce QOSC2 cs (p,x++[b],xs) bs') (U.uncons bs)

transduce QDCS cs t@(p,x,xs) ('0':>bs) = makeCharsetDesignation p Special : transduce Q0 cs t bs
transduce QDCS cs t@(p,x,xs) ('A':>bs) = makeCharsetDesignation p UK : transduce Q0 cs t bs
transduce QDCS cs t@(p,x,xs) ('B':>bs) = makeCharsetDesignation p USASCII : transduce Q0 cs t bs

transduce QESCSP _ t ('F':>bs) = transduce Q0 C0 t bs
transduce QESCSP _ t ('G':>bs) = transduce Q0 C1 t bs

transduce QCSI cs t@(p,x,xs) ('A':>bs) = CUU (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('B':>bs) = CUD (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('C':>bs) = CUF (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('D':>bs) = CUB (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('E':>bs) = CNL (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('F':>bs) = CPL (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('G':>bs) = CHA (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('H':>bs) = makeCUP (xs++[x]) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('I':>bs) = CHT (fromMaybe 1 $ readMaybe x) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) ('m':>bs) = makeSGR (xs++[x]) : transduce Q0 cs t bs
transduce QCSI cs t@(p,x,xs) (';':>bs) = transduce QCSI cs (p,"",xs++[x]) bs
transduce QCSI cs t@(p,x,xs) (b:>bs) | isDigit b = transduce QCSI cs (p,x++[b],xs) bs
                                     | otherwise = transduce Q0 cs t bs
transduce _ cs t (b:>bs) = transduce Q0 cs t bs

makeCUP :: [String] -> Command
makeCUP [] = CUP 1 1
makeCUP [r] = CUP (fromMaybe 1 $ readMaybe r) 1
makeCUP (r:c:_) = CUP (fromMaybe 1 $ readMaybe r) (fromMaybe 1 $ readMaybe c)

makeOSC :: [String] -> Command
makeOSC [] = Noop
makeOSC [_] = Noop
makeOSC ("0":t:_) = SetIconTitle t
makeOSC _ = Noop

makeCharsetDesignation :: String -> CharacterSet -> Command
makeCharsetDesignation g cs = case g of
                                  "(" -> SetCharset G0 cs
                                  ")" -> SetCharset G1 cs
                                  "*" -> SetCharset G2 cs
                                  "+" -> SetCharset G3 cs
                                  "-" -> SetCharset G1 cs
                                  "." -> SetCharset G2 cs
                                  "/" -> SetCharset G3 cs
                                  _   -> Noop

makeSGR :: [String] -> Command
makeSGR = SGR . transduceSGR QSGR0 [] . mapMaybe (readMaybe :: String -> Maybe Int)
    where transduceSGR _ _ [] = []
          transduceSGR QSGR0 l (0:xs) = Reset : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (1:xs) = Bold True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (2:xs) = Faint : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (3:xs) = Italic True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (4:xs) = Underscore True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (5:xs) = Blink True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (7:xs) = Reverse True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (8:xs) = Invisible True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (9:xs) = Strikethrough True : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (21:xs) = DoubleUnderline : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (22:xs) = Bold False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (23:xs) = Italic False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (24:xs) = Underscore False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (25:xs) = Blink False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (27:xs) = Reverse False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (28:xs) = Invisible False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (29:xs) = Strikethrough False : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (30:xs) = Foreground (Color256 0) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (31:xs) = Foreground (Color256 1) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (32:xs) = Foreground (Color256 2) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (33:xs) = Foreground (Color256 3) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (34:xs) = Foreground (Color256 4) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (35:xs) = Foreground (Color256 5) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (36:xs) = Foreground (Color256 6) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (37:xs) = Foreground (Color256 7) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (38:xs) = transduceSGR QSGR38 l xs
          transduceSGR QSGR0 l (39:xs) = Foreground Default : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (40:xs) = Background (Color256 0) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (41:xs) = Background (Color256 1) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (42:xs) = Background (Color256 2) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (43:xs) = Background (Color256 3) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (44:xs) = Background (Color256 4) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (45:xs) = Background (Color256 5) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (46:xs) = Background (Color256 6) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (47:xs) = Background (Color256 7) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (48:xs) = transduceSGR QSGR48 l xs
          transduceSGR QSGR0 l (49:xs) = Background Default : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (90:xs) = Foreground (Color256 0) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (91:xs) = Foreground (Color256 1) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (92:xs) = Foreground (Color256 2) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (93:xs) = Foreground (Color256 3) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (94:xs) = Foreground (Color256 4) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (95:xs) = Foreground (Color256 5) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (96:xs) = Foreground (Color256 6) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (97:xs) = Foreground (Color256 7) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (100:xs) = Background (Color256 0) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (101:xs) = Background (Color256 1) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (102:xs) = Background (Color256 2) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (103:xs) = Background (Color256 3) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (104:xs) = Background (Color256 4) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (105:xs) = Background (Color256 5) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (106:xs) = Background (Color256 6) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (107:xs) = Background (Color256 7) : transduceSGR QSGR0 l xs
          transduceSGR QSGR0 l (_:xs) = transduceSGR QSGR0 l xs

          transduceSGR QSGR38 l (5:xs) = transduceSGR QSGR38c l xs
          transduceSGR QSGR38 l (2:xs) = transduceSGR QSGR38r l xs
          transduceSGR QSGR38c l (x:xs) = Foreground (Color256 x) : transduceSGR QSGR0 l xs
          transduceSGR QSGR38r _ (r:xs) = transduceSGR QSGR38g [r] xs
          transduceSGR QSGR38g [r] (g:xs) = transduceSGR QSGR38b [r,g] xs
          transduceSGR QSGR38b [r,g] (b:xs) = Foreground (Truecolor r g b) : transduceSGR QSGR0 [] xs

          transduceSGR QSGR48 l (5:xs) = transduceSGR QSGR48c l xs
          transduceSGR QSGR48 l (2:xs) = transduceSGR QSGR48r l xs
          transduceSGR QSGR48c l (x:xs) = Background (Color256 x) : transduceSGR QSGR0 l xs
          transduceSGR QSGR48r _ (r:xs) = transduceSGR QSGR48g [r] xs
          transduceSGR QSGR48g [r] (g:xs) = transduceSGR QSGR48b [r,g] xs
          transduceSGR QSGR48b [r,g] (b:xs) = Background (Truecolor r g b) : transduceSGR QSGR0 [] xs

          transduceSGR _ _ (_:xs) = transduceSGR QSGR0 [] xs
