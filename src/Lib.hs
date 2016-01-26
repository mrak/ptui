module Lib
    ( ptui
    ) where

import Args
import Settings
import qualified Data.Ini as I (readIniFile)

ptui :: Args -> IO ()
ptui a = do
  configContents <- I.readIniFile (config a)
  let settings = case configContents of
                      Left s -> error s
                      Right i -> fromINI i
  print settings
