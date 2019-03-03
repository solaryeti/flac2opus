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
import           System.Directory (createDirectoryIfMissing, removeFile, doesFileExist, copyFile)
import           System.Exit (ExitCode(..), exitFailure)
import           System.FilePath (replaceExtension, takeDirectory, isAbsolute, dropDrive, (</>))
import qualified System.FilePath as FP (FilePath)
import           System.Posix.Signals (installHandler, Handler(..), sigINT)
import           Turtle hiding ((</>))


opusfile :: FP.FilePath -> FP.FilePath -> FP.FilePath
opusfile parentPath file = joinPath parentPath $ replaceExtension file "opus"

joinPath :: FP.FilePath -> FP.FilePath -> FP.FilePath
joinPath parentPath file
  | isAbsolute file = parentPath </> dropDrive file
  | otherwise = parentPath </> file

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

coverArtFiles :: FilePath -> FilePath -> IO [FilePath]
coverArtFiles src dest = do
    artFiles <- fold (find (suffix (".jpg" <|> ".png")) src) Fold.list
    filterM (missingArtFile dest) artFiles
  where
    missingArtFile dst file = (doesFileExist $ joinPath (convertFilePath dst) (convertFilePath file)) >>= return . not

main :: IO ()
main = do
    (src, dest) <- options "Convert flac files to opus file" parser

    -- Encode flac files to opus
    putStrLn "Generating list of flac files to encode..."
    files <- filesToConvert src dest
    putStrLn $ "Found " <> T.pack (show (length files)) <> " files to encode."
    displayConsoleRegions $ do
      pg <- newProgressBar def { pgWidth = 100
                               , pgTotal = if length files > 0 then (toInteger $ length files) else 1
                               , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                               }
      mapM_ (encodeFileWithProgress dest pg) files
      liftIO $ complete pg

    -- Copy cover art for opus files
    putStrLn "Searching for cover art files..."
    artFiles <- coverArtFiles src dest
    putStrLn $ "Found " <> T.pack (show (length artFiles)) <> " cover art files to copy."
    displayConsoleRegions $ do
      pg <- newProgressBar def { pgWidth = 100
                               , pgTotal = if length artFiles > 0 then (toInteger $ length artFiles) else 1
                               , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                               }
      mapM_ (copyArtWithProgress dest pg) artFiles
      liftIO $ complete pg

  where
    encodeFileWithProgress dest pg file = do
      convertFile dest file
      barComplete <- isComplete pg
      unless barComplete $ do
        tick pg
    copyArtWithProgress dest pg file = do
      let destPath = (joinPath (convertFilePath dest) (convertFilePath file))
      createDirectoryIfMissing True (takeDirectory destPath)
      copyFile (convertFilePath file) destPath
      barComplete <- isComplete pg
      unless barComplete $ do
        tick pg
