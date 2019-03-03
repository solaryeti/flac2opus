module Main where

import Protolude
import Lib
import Turtle

main :: IO ()
main = do
  (src, dest) <- options "Convert flac files to opus file" parser
  run src dest


parser :: Parser (Turtle.FilePath, Turtle.FilePath)
parser = (,) <$> argPath "src"  "The source directory"
             <*> argPath "dest" "The top-level destination directory"
