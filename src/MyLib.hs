{-# LANGUAGE OverloadedStrings #-}
module MyLib (qrFilter) where

import qualified Data.Text as T
import Text.Pandoc.Builder
import System.Process
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import Text.Printf (printf, hPrintf)
import System.IO.Temp
import Text.Pandoc.JSON (toJSONFilter)



qrFilter :: IO ()
qrFilter = toJSONFilter $ qrFilter_ getTmpPath

qrFilter_ :: IO String -> Inline -> IO Inline
qrFilter_ getPath lnk@(Link attr@(_,classes,_) alt (tgt,titleText)) = do
  if "qr" `elem` classes
  then do
    path <- getPath
    procHandle <- runCommand $ printf "qrencode -t SVG -o '%s' '%s'" path tgt
    exitCode <- waitForProcess procHandle
    case exitCode of
      ExitSuccess -> do
        return $ Image attr alt (T.pack path,titleText)
      ExitFailure n -> do
        error $ printf "Failed to create QR code with error %d: %s" path n
  else
    return lnk
qrFilter_ _ inline = return inline


getTmpPath :: IO String
getTmpPath = do
  emptySystemTempFile "qr.svg"
