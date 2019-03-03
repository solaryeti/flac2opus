{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}

module Lib
    ( run
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Foldl as Fold (list)
import           Control.Monad (filterM, when)
import qualified Data.Text as T
import           Lib.Prelude hiding (find)
import           System.Console.AsciiProgress
import           System.Directory (createDirectoryIfMissing, removeFile, doesFileExist, copyFile)
import           System.Exit (ExitCode(..), exitFailure)
import           System.FilePath (replaceExtension, takeDirectory, isAbsolute, dropDrive, (</>))
import qualified System.FilePath as FP (FilePath)
import           System.Posix.Signals (installHandler, Handler(..), sigINT)
import           Turtle (format, fp, procStrictWithErr, suffix, find)
import qualified Turtle (fold, FilePath)

opusfile :: FP.FilePath -> FP.FilePath -> FP.FilePath
opusfile parentPath file = joinPath parentPath $ replaceExtension file "opus"

joinPath :: FP.FilePath -> FP.FilePath -> FP.FilePath
joinPath parentPath file
  | isAbsolute file = parentPath </> dropDrive file
  | otherwise = parentPath </> file

convertFilePath :: Turtle.FilePath -> FP.FilePath
convertFilePath p = T.unpack (format fp p)

cleanup :: FP.FilePath -> IO ()
cleanup file = do
  fileExists <- doesFileExist file
  when fileExists $ do
    putStrLn $ "\nProcess failed. Cleaning up " <> T.pack file
    removeFile file

filesToConvert :: Turtle.FilePath -> Turtle.FilePath -> IO [Turtle.FilePath]
filesToConvert src dest = do
    flacFiles <- Turtle.fold (find (suffix ".flac") src) Fold.list
    filterM (missingOpusFile dest) flacFiles
  where
    missingOpusFile dst file = not <$> doesFileExist (opusfile (convertFilePath dst) (convertFilePath file))

convertFile :: Turtle.FilePath -> Turtle.FilePath -> IO ()
convertFile dest file  = do
  let outfile = opusfile (convertFilePath dest) (convertFilePath file)
  _ <- installHandler sigINT (Catch $ cleanup outfile) Nothing
  createDirectoryIfMissing True (takeDirectory outfile)
  (code, _, opuserr) <- procStrictWithErr "opusenc"
                        [ "--bitrate"
                        , "128"
                        , "--vbr"
                        , format fp file
                        , T.pack outfile ]
                        empty
  unless (code == ExitSuccess) $ do
    -- Give some time for our cleanup operations in the
    -- installHandler to do their thing
    threadDelay 300000
    putStrLn $ T.pack ("Failed encoding: " ++ outfile)
    putStrLn ("opusenc output:" :: Text)
    putStrLn opuserr
    exitFailure

coverArtFiles :: Turtle.FilePath -> Turtle.FilePath -> IO [Turtle.FilePath]
coverArtFiles src dest = do
    artFiles <- Turtle.fold (find (suffix (".jpg" <|> ".png")) src) Fold.list
    filterM (missingArtFile dest) artFiles
  where
    missingArtFile dst file = not <$> doesFileExist (joinPath (convertFilePath dst) (convertFilePath file))

run :: Turtle.FilePath -> Turtle.FilePath -> IO ()
run src dest = do
    -- Encode flac files to opus
    putStrLn ("Generating list of flac files to encode..." :: Text)
    files <- filesToConvert src dest
    putStrLn $ "Found " <> T.pack (show (length files)) <> " files to encode."
    displayConsoleRegions $ do
      pg <- newProgressBar def { pgWidth = 100
                               , pgTotal = if not (null files) then toInteger $ length files else 1
                               , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                               }
      mapM_ (encodeFileWithProgress dest pg) files
      liftIO $ complete pg

    -- Copy cover art for opus files
    putStrLn ("Searching for cover art files..." :: Text)
    artFiles <- coverArtFiles src dest
    putStrLn $ "Found " <> T.pack (show (length artFiles)) <> " cover art files to copy."
    displayConsoleRegions $ do
      pg <- newProgressBar def { pgWidth = 100
                               , pgTotal = if not (null artFiles) then toInteger $ length artFiles else 1
                               , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                               }
      mapM_ (copyArtWithProgress dest pg) artFiles
      liftIO $ complete pg

  where
    encodeFileWithProgress dest' pg file = do
      convertFile dest' file
      barComplete <- isComplete pg
      unless barComplete $ tick pg
    copyArtWithProgress dest' pg file = do
      let destPath = joinPath (convertFilePath dest') (convertFilePath file)
      createDirectoryIfMissing True (takeDirectory destPath)
      copyFile (convertFilePath file) destPath
      barComplete <- isComplete pg
      unless barComplete $ tick pg
