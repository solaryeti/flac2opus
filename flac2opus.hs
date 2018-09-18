#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import           Control.Concurrent (threadDelay)
import qualified Control.Foldl as Fold (list)
import           Control.Monad (filterM)
import           Control.Monad (when)
import qualified Data.Text as T
import           Data.Text.IO (putStrLn)
import           Prelude hiding (FilePath, putStrLn)
import           System.Console.AsciiProgress
import           System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import           System.Exit (ExitCode(..), exitFailure)
import           System.FilePath (replaceExtension, takeDirectory)
import qualified System.FilePath as FP (FilePath)
import           System.Posix.Signals (installHandler, Handler(..), sigINT)
import           Turtle

parser :: Parser (FilePath, FilePath)
parser = (,) <$> argPath "src"  "The source directory"
             <*> argPath "dest" "The top-level destination directory"

opusfile :: FP.FilePath -> FP.FilePath -> FP.FilePath
opusfile parentPath file = parentPath <> replaceExtension file "opus"

convertFilePath :: FilePath -> FP.FilePath
convertFilePath p = T.unpack (format fp p)

cleanup :: FP.FilePath -> IO ()
cleanup file = do
  fileExists <- doesFileExist file
  when fileExists $ do
    putStrLn $ "\nProcess failed. Cleaning up " <> T.pack file
    removeFile file

filesToConvert :: FilePath -> FilePath -> IO [FilePath]
filesToConvert src dest = do
    flacFiles <- fold (find (suffix ".flac") src) Fold.list
    filterM (missingOpusFile dest) flacFiles
  where
    missingOpusFile dst file = (doesFileExist $ opusfile (convertFilePath dst) (convertFilePath file)) >>= return . not

convertFile :: FilePath -> FilePath -> IO ()
convertFile dest file  = do
  let outfile = opusfile (convertFilePath dest) (convertFilePath file)
  _ <- installHandler sigINT (Catch $ cleanup outfile) Nothing
  createDirectoryIfMissing True (takeDirectory outfile)
  (code, _, opuserr) <- procStrictWithErr "opusenc" [ "--bitrate", "128"
                                  , "--vbr"
                                  , format fp file
                                  , T.pack outfile ] empty
  unless (code == ExitSuccess) $ do
    -- Give some time for our cleanup operations in the
    -- installHandler to do their thing
    threadDelay 300000
    putStrLn $ T.pack ("Failed encoding: " ++ outfile)
    putStrLn "opusenc output:"
    putStrLn opuserr
    exitFailure

main :: IO ()
main = do
    (src, dest) <- options "Convert flac files to opus file" parser
    files <- filesToConvert src dest
    displayConsoleRegions $ do
      pg <- newProgressBar def { pgWidth = 100
                               , pgTotal = if length files > 0 then (toInteger $ length files) else 1
                               , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                               }
      mapM_ (go dest pg) files
      liftIO $ complete pg
  where
    go dest pg file = do
      convertFile dest file
      barComplete <- isComplete pg
      unless barComplete $ do
        tick pg
