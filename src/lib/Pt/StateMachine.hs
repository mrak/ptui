{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Pt.StateMachine where

import Data.ByteString.Lazy.Char8

data State = Q0
           | QESC
           | QSGR
           | QSGR2
           | QSGR3
           | QSGR4
           | QSGREND

data SGR = Bold Bool
         | Faint
         | Blink Bool
         | Underscore Bool
         | Italic Bool
         | Reverse Bool
         | Foreground Int
         | Background Int
         | Invisible Bool
         | Strikethrough Bool
         | DoubleUnderline
         | Reset
         deriving Show

data Command = Noop
             | Output Char
             | SGRs [SGR]
             | End
             deriving Show

pattern b :> bs <- (uncons -> Just (b,bs))
pattern Empty   <- (uncons -> Nothing)

transduce :: State -> Command -> ByteString -> [Command]
transduce _ _ Empty         = []
transduce Q0 _ ('\x1b':>bs) = transduce QESC Noop bs
transduce Q0 _ (b:>bs)      = Output 'x' : transduce Q0 Noop bs

transduce QESC _ ('[':>bs) = transduce QSGR (SGRs []) bs

transduce QSGR  (SGRs l) ('0':>bs) = transduce QSGREND (SGRs (l ++ [Reset])) bs
transduce QSGR  (SGRs l) ('1':>bs) = transduce QSGREND (SGRs (l ++ [Bold True])) bs
transduce QSGR  c        ('2':>bs) = transduce QSGR2 c bs
transduce QSGR  c        ('3':>bs) = transduce QSGR3 c bs
transduce QSGR  c        ('4':>bs) = transduce QSGR4 c bs
transduce QSGR  (SGRs l) ('5':>bs) = transduce QSGREND (SGRs (l ++ [Blink True])) bs
transduce QSGR  (SGRs l) ('7':>bs) = transduce QSGREND (SGRs (l ++ [Reverse True])) bs
transduce QSGR  (SGRs l) ('8':>bs) = transduce QSGREND (SGRs (l ++ [Invisible True])) bs
transduce QSGR  (SGRs l) ('9':>bs) = transduce QSGREND (SGRs (l ++ [Strikethrough True])) bs

transduce QSGR2 (SGRs l) ('1':>bs) = transduce QSGREND (SGRs (l ++ [DoubleUnderline])) bs
transduce QSGR2 (SGRs l) ('2':>bs) = transduce QSGREND (SGRs (l ++ [Bold False])) bs
transduce QSGR2 (SGRs l) ('3':>bs) = transduce QSGREND (SGRs (l ++ [Italic False])) bs
transduce QSGR2 (SGRs l) ('4':>bs) = transduce QSGREND (SGRs (l ++ [Underscore False])) bs
transduce QSGR2 (SGRs l) ('5':>bs) = transduce QSGREND (SGRs (l ++ [Blink False])) bs
transduce QSGR2 (SGRs l) ('7':>bs) = transduce QSGREND (SGRs (l ++ [Reverse False])) bs
transduce QSGR2 (SGRs l) ('8':>bs) = transduce QSGREND (SGRs (l ++ [Invisible False])) bs
transduce QSGR2 (SGRs l) ('9':>bs) = transduce QSGREND (SGRs (l ++ [Strikethrough False])) bs
transduce QSGR2 (SGRs l) (';':>bs) = transduce QSGR (SGRs (l ++ [Faint])) bs
transduce QSGR2 (SGRs l) ('m':>bs) = SGRs (l ++ [Faint]) : transduce Q0 Noop bs

transduce QSGR3 (SGRs l) ('0':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 0])) bs
transduce QSGR3 (SGRs l) ('1':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 1])) bs
transduce QSGR3 (SGRs l) ('2':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 2])) bs
transduce QSGR3 (SGRs l) ('3':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 3])) bs
transduce QSGR3 (SGRs l) ('4':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 4])) bs
transduce QSGR3 (SGRs l) ('5':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 5])) bs
transduce QSGR3 (SGRs l) ('6':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 6])) bs
transduce QSGR3 (SGRs l) ('7':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 7])) bs
transduce QSGR3 (SGRs l) ('9':>bs) = transduce QSGREND (SGRs (l ++ [Foreground 9])) bs
transduce QSGR3 (SGRs l) (';':>bs) = transduce QSGR (SGRs (l ++ [Italic True])) bs
transduce QSGR3 (SGRs l) ('m':>bs) = SGRs (l ++ [Italic True]) : transduce Q0 Noop bs

transduce QSGR4 (SGRs l) ('0':>bs) = transduce QSGREND (SGRs (l ++ [Background 0])) bs
transduce QSGR4 (SGRs l) ('1':>bs) = transduce QSGREND (SGRs (l ++ [Background 1])) bs
transduce QSGR4 (SGRs l) ('2':>bs) = transduce QSGREND (SGRs (l ++ [Background 2])) bs
transduce QSGR4 (SGRs l) ('3':>bs) = transduce QSGREND (SGRs (l ++ [Background 3])) bs
transduce QSGR4 (SGRs l) ('4':>bs) = transduce QSGREND (SGRs (l ++ [Background 4])) bs
transduce QSGR4 (SGRs l) ('5':>bs) = transduce QSGREND (SGRs (l ++ [Background 5])) bs
transduce QSGR4 (SGRs l) ('6':>bs) = transduce QSGREND (SGRs (l ++ [Background 6])) bs
transduce QSGR4 (SGRs l) ('7':>bs) = transduce QSGREND (SGRs (l ++ [Background 7])) bs
transduce QSGR4 (SGRs l) ('9':>bs) = transduce QSGREND (SGRs (l ++ [Background 9])) bs
transduce QSGR4 (SGRs l) (';':>bs) = transduce QSGR (SGRs (l ++ [Underscore True])) bs
transduce QSGR4 (SGRs l) ('m':>bs) = SGRs (l ++ [Underscore True]) : transduce Q0 Noop bs

transduce QSGREND c (';':>bs) = transduce QSGR c bs
transduce QSGREND c ('m':>bs) = c : transduce Q0 Noop bs

transduce _ _ (b :> bs)       = transduce Q0 Noop bs
