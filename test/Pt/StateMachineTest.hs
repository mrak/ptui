{-# LANGUAGE OverloadedStrings #-}
module Pt.StateMachineTest where

import Pt.StateMachine
import Test.HUnit

import Data.ByteString.Lazy.Char8 as B

tests = "StateMachine" ~: test $ (($ runFSM) <$> tests') ++ (($ runFSMC1) <$> tests') ++ [c1]
    where tests' = [ basics
                   , escapes
                   ]


basics f = "Basics" ~: test [ "BEL" ~: [BEL] ~=? f "\a"
                            , "BS" ~: [BS] ~=? f "\b"
                            , "HT" ~: [HT] ~=? f "\t"
                            , "LF" ~: [LF] ~=? f "\n"
                            , "VT" ~: [VT] ~=? f "\v"
                            , "FF" ~: [FF] ~=? f "\f"
                            , "CR" ~: [CR] ~=? f "\r"
                            ]

escapes f = "ESC+" ~: test [ "D -> IND" ~: [IND] ~=? f (pack ['\x1b','D'])
                           , "E -> NEL" ~: [NEL] ~=? f (pack ['\x1b','E'])
                           , "H -> HTS" ~: [HTS] ~=? f (pack ['\x1b','H'])
                           , "M -> RI" ~: [RI] ~=? f (pack ['\x1b','M'])
                           ]

c1 = "C1 (S8C1T)" ~: test [ "IND" ~: [IND] ~=? runFSMC1 "\x84"
                          , "NEL" ~: [NEL] ~=? runFSMC1 "\x85"
                          , "HTS" ~: [HTS] ~=? runFSMC1 "\x88"
                          , "RI" ~: [RI] ~=? runFSMC1 "\x8d"
                          ]
