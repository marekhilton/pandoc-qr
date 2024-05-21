{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
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
  Figure figAttr capt [Plain [QRImage attr alt (tgt,titleText)]] ->
    do
      let config = QRConfig tgt titleText attr alt
      qr <- generateQRImage getPath config
      return $ Figure figAttr capt [Plain [qr]]
  Para [QRImage attr alt (tgt,titleText)] ->
    do
      let config = QRConfig tgt titleText attr alt
      qr <- generateQRImage getPath config
      return $ Figure nullAttr emptyCaption [Plain [qr]]
  _notLink ->
    return blk

pattern QRImage :: Attr -> [Inline] -> (T.Text,T.Text) -> Inline
pattern QRImage x y z <- (matchQRImage -> Just (x,y,z))

matchQRImage :: Inline -> Maybe (Attr,[Inline],(T.Text,T.Text))
matchQRImage (Image attr alt tgtTitle)
  | attr `isClass` "qr" = Just (attr,alt,tgtTitle)
matchQRImage _ = Nothing

isClass :: Attr -> T.Text -> Bool
isClass (_,classes,_) c =
  c `elem` classes
  

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
