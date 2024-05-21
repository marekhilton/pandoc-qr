{-# LANGUAGE OverloadedStrings #-}
module MyLib (qrFilter) where

import qualified Data.Text as T
import Text.Pandoc.Builder
import System.Process
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import Text.Printf (printf)
import System.IO.Temp
import Text.Pandoc.JSON (toJSONFilter)


data QRConfig = QRConfig
  { target :: T.Text
  , title :: T.Text
  , attributes :: Attr
  , alternate :: [Inline] }
    

qrFilter :: IO ()
qrFilter = toJSONFilter $ qrFilter_ getTmpPath

qrFilter_ :: IO String -> Block -> IO Block
qrFilter_ getPath blk =
  case blk of
  Figure figAttr capt [Plain [Image attr@(_,classes,_) alt (tgt,titleText)]]
    | "qr" `elem` classes -> do
      let config = QRConfig tgt titleText attr alt
      qr <- generateQRImage getPath config
      return $ Figure figAttr capt [Plain [qr]]
  Para [Image attr@(_,classes,_) alt (tgt,titleText)]
    | "qr" `elem` classes -> do
      let config = QRConfig tgt titleText attr alt
      qr <- generateQRImage getPath config
      return $ Figure nullAttr emptyCaption [Plain [qr]]
  _notLink ->
    return blk

generateQRImage :: IO String -> QRConfig -> IO Inline
generateQRImage getPath config = do
      path <- getPath
      procHandle <- runCommand $ printf "qrencode -t SVG -o '%s' '%s'" path (target config)
      exitCode <- waitForProcess procHandle
      case exitCode of
        ExitSuccess -> do
          return $ Image (attributes config) (alternate config) (T.pack path,title config)
        ExitFailure n -> do
          error $ printf "Failed to create QR code with error %d: %s" path n


getTmpPath :: IO String
getTmpPath = do
  emptySystemTempFile "qr.svg"
