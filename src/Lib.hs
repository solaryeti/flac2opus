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
import           Turtle (format, fp, fromString, procStrictWithErr, suffix, find)
import qualified Turtle (fold, FilePath)
import qualified Control.Concurrent.PooledIO.Final as Pool


joinPath :: FP.FilePath -> FP.FilePath -> FP.FilePath
joinPath parentPath file
  | isAbsolute file = parentPath </> dropDrive file
  | otherwise = parentPath </> file

convertFilePath :: Turtle.FilePath -> FP.FilePath
convertFilePath = T.unpack . format fp

cleanup :: FP.FilePath -> IO ()
cleanup file = do
  fileExists <- doesFileExist file
  when fileExists $ do
    putStrLn $ "\nProcess failed. Cleaning up " <> T.pack file
    removeFile file

filesToConvert :: FP.FilePath -> FP.FilePath -> IO [FP.FilePath]
filesToConvert src dest = do
    flacFiles <- Turtle.fold (find (suffix ".flac") (fromString src)) Fold.list
    fmap convertFilePath <$> filterM (missingOpusFile dest) flacFiles
  where
    missingOpusFile dst file = not <$> doesFileExist (joinPath dst (replaceExtension (convertFilePath file) "opus"))

convertFile :: FP.FilePath -> FP.FilePath -> IO ()
convertFile dest file  = do
  let outfile = replaceExtension dest "opus"
  _ <- installHandler sigINT (Catch $ cleanup outfile) Nothing
  createDirectoryIfMissing True (takeDirectory outfile)
  (code, _, opuserr) <- procStrictWithErr "opusenc"
                        [ "--bitrate"
                        , "128"
                        , "--vbr"
                        , T.pack file
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

coverArtFiles :: FP.FilePath -> FP.FilePath -> IO [FP.FilePath]
coverArtFiles src dest = do
    artFiles <- Turtle.fold (find (suffix (".jpg" <|> ".png")) (fromString src)) Fold.list
    fmap convertFilePath <$> filterM (missingArtFile dest) artFiles
  where
    missingArtFile dst file = not <$> doesFileExist (joinPath dst (convertFilePath file))

run :: Text -> Text -> Int -> IO ()
run src dest workers = do
    -- Encode flac files to opus
    doWithProgress filesToConvert convertFile "flac files"
    -- Copy cover art for opus files
    doWithProgress coverArtFiles (flip copyFile) "cover art"

 where
    doWithProgress :: (FP.FilePath -> FP.FilePath -> IO [FP.FilePath])
                   -> (FP.FilePath -> FP.FilePath -> IO ())
                   -> Text
                   -> IO ()
    doWithProgress findFunc actionFunc description = do
      putStrLn $ "Finding " <> description <> "..."
      files <- findFunc (T.unpack src) (T.unpack dest)
      putStrLn $ "Found " <> T.pack (show (length files)) <> " files."
      displayConsoleRegions $ do
        pg <- newProgressBar def { pgWidth = 100
                                 , pgTotal = if not (null files) then toInteger $ length files else 1
                                 , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                                 }
        mapPool_ workers (applyProgress actionFunc (T.unpack dest) pg) files
        liftIO $ complete pg

    applyProgress :: (FP.FilePath -> FP.FilePath -> IO ()) -> FP.FilePath -> ProgressBar ->  FP.FilePath -> IO ()
    applyProgress f dest' pg file = do
      let destPath = joinPath dest' file
      createDirectoryIfMissing True (takeDirectory destPath)
      f destPath file
      barComplete <- isComplete pg
      unless barComplete $ tick pg

mapPool_ :: (Traversable t, NFData b)
         => Int
         -> (a -> IO b)
         -> t a
         -> IO ()
mapPool_ n f = Pool.runLimited n . traverse_ (Pool.fork . f)
