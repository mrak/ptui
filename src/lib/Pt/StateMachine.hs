{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Pt.StateMachine where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Data.List (foldl',uncons)
import Data.Maybe (mapMaybe,fromMaybe)
import Text.Read (readMaybe)

data State = Q0
           | QESC
           | QCSI
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

data Color = Color256 Int
           | Trucolor Int Int Int
           | Default
           deriving Show

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
             deriving Show

data Command = Noop
             | Output Char
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
             deriving Show

pattern b :> bs <- (B.uncons -> Just (b,bs))
pattern Empty   <- (B.uncons -> Nothing)

transduce :: State -> (String,[String]) -> B.ByteString -> [Command]
transduce _ _ Empty         = []
transduce Q0 t ('\x1b':>bs) = transduce QESC t bs
transduce Q0 t (b:>bs)      = Output b : transduce Q0 t bs

transduce QESC _ ('[':>bs) = transduce QCSI ("",[]) bs

transduce QCSI (p,ps) ('A':>bs) = CUU (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('B':>bs) = CUD (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('C':>bs) = CUF (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('D':>bs) = CUB (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('E':>bs) = CNL (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('F':>bs) = CPL (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('G':>bs) = CHA (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('H':>bs) = makeCUP (ps++[p]) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('I':>bs) = CHT (fromMaybe 1 $ readMaybe p) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) ('m':>bs) = makeSGR (ps++[p]) : transduce Q0 ("",[]) bs
transduce QCSI (p,ps) (';':>bs) = transduce QCSI ("",ps++[p]) bs
transduce QCSI (p,ps) (b:>bs) | isDigit b = transduce QCSI (p++[b],ps) bs
                              | otherwise = transduce Q0 (p,ps) bs
transduce _ t (b:>bs) = transduce Q0 t bs

makeCUP :: [String] -> Command
makeCUP [] = CUP 1 1
makeCUP [r] = CUP (fromMaybe 1 $ readMaybe r) 1
makeCUP (r:c:_) = CUP (fromMaybe 1 $ readMaybe r) (fromMaybe 1 $ readMaybe c)

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
          transduceSGR QSGR38b [r,g] (b:xs) = Foreground (Trucolor r g b) : transduceSGR QSGR0 [] xs

          transduceSGR QSGR48 l (5:xs) = transduceSGR QSGR48c l xs
          transduceSGR QSGR48 l (2:xs) = transduceSGR QSGR48r l xs
          transduceSGR QSGR48c l (x:xs) = Background (Color256 x) : transduceSGR QSGR0 l xs
          transduceSGR QSGR48r _ (r:xs) = transduceSGR QSGR48g [r] xs
          transduceSGR QSGR48g [r] (g:xs) = transduceSGR QSGR48b [r,g] xs
          transduceSGR QSGR48b [r,g] (b:xs) = Background (Trucolor r g b) : transduceSGR QSGR0 [] xs

          transduceSGR _ _ (_:xs) = transduceSGR QSGR0 [] xs
