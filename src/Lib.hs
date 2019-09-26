{-|
Module      : Lib
Description : Main library for flac2opus

-}

module Lib
  ( Opts(..)
  , SrcFilePath(..)
  , DstFilePath(..)
  , run
  )
where

import           Lib.Prelude

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( filterM
                                                , when
                                                )
import           System.Console.AsciiProgress
import           System.Directory               ( createDirectoryIfMissing
                                                , removeFile
                                                , doesFileExist
                                                , copyFile
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitFailure
                                                )
import           System.FilePath                ( FilePath
                                                , replaceExtension
                                                , takeDirectory
                                                , isAbsolute
                                                , dropDrive
                                                , (</>)
                                                )
import           System.Posix.Signals           ( installHandler
                                                , Handler(..)
                                                , sigINT
                                                )
import           Turtle                         ( procStrictWithErr )

import qualified Data.Text                     as T
import qualified Control.Concurrent.PooledIO.Final
                                               as Pool
import qualified System.FilePath.Find          as F

newtype SrcFilePath = SrcFilePath { fromSrcFilePath :: FilePath }
newtype DstFilePath = DstFilePath { fromDstFilePath :: FilePath }

data Opts = Opts
  { oWorkers :: Int
  , oVerbose :: Bool
  , oSrc     :: SrcFilePath
  , oDst     :: DstFilePath
  }

-- | Create a version of `System.FilePath.combine` that prepends the
-- given `parentPath` even when the `file` is absolute rather than
-- just returning the `file` unchanged.
joinPath :: FilePath -> FilePath -> FilePath
joinPath parentPath file | isAbsolute file = parentPath </> dropDrive file
                         | otherwise       = parentPath </> file

-- | Find the flac files that are present under the `src`
-- directory but missing from the `dst` directory.
filesToConvert :: SrcFilePath -> DstFilePath -> IO [FilePath]
filesToConvert (SrcFilePath src) (DstFilePath dst) = do
  flacFiles <- F.find (pure True) (F.extension F.==? ".flac") src
  filterM (missingOpusFile dst) flacFiles
 where
  missingOpusFile :: FilePath -> FilePath -> IO Bool
  missingOpusFile dst' file =
    not <$> doesFileExist (joinPath dst' (replaceExtension file "opus"))

-- | Convert a flac file to opus, prepending the `dst` to the outfile.
convertFile :: SrcFilePath -> DstFilePath -> IO ()
convertFile (SrcFilePath src) (DstFilePath dst) = do
  _ <- installHandler sigINT (Catch $ cleanup outfile) Nothing
  createDirectoryIfMissing True (takeDirectory outfile)
  (code, _, opuserr) <- procStrictWithErr
    "opusenc"
    ["--bitrate", "128", "--vbr", T.pack src, T.pack outfile]
    empty
  unless (code == ExitSuccess) $ fatalError opuserr
 where
  outfile :: FilePath
  outfile = replaceExtension (joinPath dst src) "opus"

  fatalError :: Text -> IO ()
  fatalError err = do
    -- Give some time for our cleanup operations in the
    -- installHandler to do their thing
    threadDelay 300000
    putStrLn $ "Failed encoding: " ++ outfile
    putStrLn ("opusenc output:" :: Text)
    putStrLn err
    exitFailure

  cleanup :: FilePath -> IO ()
  cleanup file = do
    fileExists <- doesFileExist file
    when fileExists $ do
      putStrLn $ "\nProcess failed. Cleaning up " <> T.pack file
      removeFile file

-- | Find the cover art files that are present under the `src`
-- directory but missing from the `dst` directory.
coverArtFiles :: SrcFilePath -> DstFilePath -> IO [FilePath]
coverArtFiles (SrcFilePath src) (DstFilePath dst) = do
  artFiles <- F.find (pure True)
                     (F.extension F.==? ".jpg" F.||? F.extension F.==? ".png")
                     src
  filterM (missingArtFile dst) artFiles
 where
  missingArtFile :: FilePath -> FilePath -> IO Bool
  missingArtFile dst' file = not <$> doesFileExist (joinPath dst' file)

run :: Opts -> IO ()
run (Opts workers verbose src dst) = do
    -- Encode flac files to opus
  doWithProgress filesToConvert convertFile "flac files"
  putStrLn T.empty
  -- Copy cover art for opus files
  doWithProgress coverArtFiles copy "cover art"

 where
  doWithProgress
    :: (SrcFilePath -> DstFilePath -> IO [FilePath])  -- ^ Function to find files
    -> (SrcFilePath -> DstFilePath -> IO ())          -- ^ Function to apply to found files
    -> Text                                           -- ^ Description of files to print in user messages
    -> IO ()
  doWithProgress findFunc actionFunc description = do
    when verbose $ putStrLn $ "Finding " <> description <> "..."
    files <- findFunc src dst
    when verbose $ printFiles files

    displayConsoleRegions $ do
      pg <- newProgressBar def
        { pgWidth = 100
        , pgTotal = if not (null files) then toInteger $ length files else 1
        , pgOnCompletion = Just
                           $  "Done :percent after :elapsed seconds ("
                           ++ T.unpack description
                           ++ ")"
        }
      mapPool_ workers
               (applyProgress pg actionFunc dst)
               (fmap SrcFilePath files)
      liftIO $ complete pg

  applyProgress
    :: ProgressBar
    -> (SrcFilePath -> DstFilePath -> IO ())
    -> DstFilePath
    -> SrcFilePath
    -> IO ()
  applyProgress pg f dst' src' = do
    createDirectoryIfMissing True $ takeDirectory
      (joinPath (fromDstFilePath dst') (fromSrcFilePath src'))
    f src' dst'
    barComplete <- isComplete pg
    unless barComplete $ tick pg

  copy :: SrcFilePath -> DstFilePath -> IO ()
  copy (SrcFilePath src') (DstFilePath dst') =
    copyFile src' $ joinPath dst' src'

  printFiles :: [FilePath] -> IO ()
  printFiles files = do
    putStrLn $ "Found " <> T.pack (show (length files)) <> " files:"
    mapM_ putStrLn files

-- | Create a pool of 'n' threads and map an IO function over a
-- traversable using that pool to run the function concurrently.
mapPool_
  :: (Traversable t, NFData b)
  => Int          -- ^ Pool size
  -> (a -> IO b)  -- ^ Function to map
  -> t a          -- ^ Traversable to map function over
  -> IO ()
mapPool_ n f = Pool.runLimited n . traverse_ (Pool.fork . f)
