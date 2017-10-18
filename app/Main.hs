module Main where

import MsonParser as MSON

import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let msonFileName = head args
  contents <- readFile msonFileName
  case MSON.msonParse aMsonTypeDecl msonFileName contents of
    Left  err    -> hPutStrLn stderr (show err)
    Right result -> putStrLn $ show result
