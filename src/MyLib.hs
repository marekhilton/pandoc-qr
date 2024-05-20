{-# LANGUAGE OverloadedStrings #-}
module MyLib (qrFilter) where

import qualified Data.Text as T
import Text.Pandoc.Builder
import System.Process
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import Text.Printf (printf)
import System.IO.Temp
import Text.Pandoc.JSON (toJSONFilter)

data QRConfig = QRConfig { url :: String
                         , title :: T.Text }


qrFilter :: IO ()
qrFilter = toJSONFilter $ qrFilter_ getTmpPath

qrFilter_ :: IO String -> Inline -> IO Inline
qrFilter_ getPath lnk@(Link (_,classes,_) _ (tgt,titleText)) = do
  if "qr" `elem` classes
  then do
    generateQR getPath (QRConfig { url = T.unpack tgt , title = titleText})
  else
    return lnk
qrFilter_ _ inline = return inline

generateQR :: IO String -> QRConfig -> IO Inline
generateQR getPath config = do
  path <- getPath
  procHandle <- runCommand $ "qrencode -o -t SVG" ++ path ++ "'" ++ url config ++ "'"
  exitCode <- waitForProcess procHandle
  case exitCode of
    ExitSuccess -> return $ Image nullAttr [] (T.pack path,title config)
    ExitFailure n -> do
      error $ printf "Failed to create QR code with error %d: %s" path n

getTmpPath :: IO String
getTmpPath = do
  emptyTempFile "qr_codes" "qr.svg"
