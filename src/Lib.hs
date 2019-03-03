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
import           System.FilePath (FilePath, replaceExtension, takeDirectory, isAbsolute, dropDrive, (</>))
import           System.Posix.Signals (installHandler, Handler(..), sigINT)
import           Turtle (format, fp, fromString, procStrictWithErr, suffix, find)
import qualified Turtle (fold, FilePath)
import qualified Control.Concurrent.PooledIO.Final as Pool

-- | Create a version of `System.FilePath.combine` that prepends the
-- given `parentPath` even when the `file` is absolute rather than
-- just returning the `file` unchanged.
joinPath :: FilePath -> FilePath -> FilePath
joinPath parentPath file
  | isAbsolute file = parentPath </> dropDrive file
  | otherwise = parentPath </> file

convertFilePath :: Turtle.FilePath -> FilePath
convertFilePath = T.unpack . format fp

cleanup :: FilePath -> IO ()
cleanup file = do
  fileExists <- doesFileExist file
  when fileExists $ do
    putStrLn $ "\nProcess failed. Cleaning up " <> T.pack file
    removeFile file

-- | Find the flac files that are present under the `src`
-- directory but missing from the `dest` directory.
filesToConvert :: FilePath -> FilePath -> IO [FilePath]
filesToConvert src dest = do
    flacFiles <- Turtle.fold (find (suffix ".flac") (fromString src)) Fold.list
    fmap convertFilePath <$> filterM (missingOpusFile dest) flacFiles
  where
    missingOpusFile dst file = not <$> doesFileExist (joinPath dst (replaceExtension (convertFilePath file) "opus"))

-- | Convert a flac file file to opus, prepending the `dest` to the outfile.
convertFile :: FilePath -> FilePath -> IO ()
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

-- | Find the cover art files that are present under the `src`
-- directory but missing from the `dest` directory.
coverArtFiles :: FilePath -> FilePath -> IO [FilePath]
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
    doWithProgress :: (FilePath -> FilePath -> IO [FilePath])  -- ^ Function to find files
                   -> (FilePath -> FilePath -> IO ())          -- ^ Action to apply to found files
                   -> Text                                     -- ^ Description of files to print in user messages
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

    applyProgress :: (FilePath -> FilePath -> IO ()) -> FilePath -> ProgressBar ->  FilePath -> IO ()
    applyProgress f dest' pg file = do
      let destPath = joinPath dest' file
      createDirectoryIfMissing True (takeDirectory destPath)
      f destPath file
      barComplete <- isComplete pg
      unless barComplete $ tick pg

-- | Create a pool of 'n' threads and map an IO function over a
-- traversable using that pool to run the function concurrently.
mapPool_ :: (Traversable t, NFData b)
         => Int
         -> (a -> IO b)
         -> t a
         -> IO ()
mapPool_ n f = Pool.runLimited n . traverse_ (Pool.fork . f)
