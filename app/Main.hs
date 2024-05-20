module Main where

import qualified MyLib (qrFilter)

main :: IO ()
main =
  MyLib.qrFilter
