#!/usr/bin/env stack
-- stack --resolver lts-12.10 script

{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E (throwTo)
import           Control.Monad (when)
import qualified Data.Text as T
import           Data.Text.IO (putStrLn)
import           Prelude hiding (FilePath, putStrLn)
import           System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import           System.Exit (ExitCode(..), exitFailure)
import           System.FilePath (makeRelative, replaceExtension, FilePath, takeDirectory, (</>))
import qualified System.IO as IO
import           System.Posix.Signals (installHandler, keyboardSignal, Handler(..), sigINT)
import qualified Turtle as Turtle (FilePath)
import           Turtle hiding (FilePath, (</>))

parser :: Parser (Turtle.FilePath, Turtle.FilePath)
parser = (,) <$> argPath "src"  "The source directory"
             <*> argPath "dest" "The top-level destination directory"

opusfile :: FilePath -> FilePath -> FilePath
opusfile parent file = parent </> replaceExtension file "opus"

convertFilePath :: Turtle.FilePath -> System.FilePath.FilePath
convertFilePath p = T.unpack (format fp p)

cleanup :: FilePath -> IO ()
cleanup file = do
  fileExists <- doesFileExist file
  when fileExists $ do
    putStrLn $ "\nProcess failed. Cleaning up " <> T.pack file
    removeFile file
    putStrLn "Exiting..."

main :: IO ()
main = do
  (src, dest) <- options "Convert flac files to opus file" parser
  sh $ do
    file <- find (suffix ".flac") src
    let outfile = opusfile (convertFilePath dest) (convertFilePath file)

    liftIO $ do
      installHandler sigINT (Catch $ cleanup outfile) Nothing
      outfileExists <- doesFileExist outfile
      unless outfileExists $ do
        putStrLn $ format ("Converting: "%fp%"") file
        putStrLn $ T.pack ("To: " ++ outfile)
        createDirectoryIfMissing True (takeDirectory outfile)
        code <- proc "opusenc" [ "--bitrate", "128"
                               , "--vbr"
                               , format fp file
                               , T.pack outfile ] empty
        unless (code == ExitSuccess) $ do
          -- Give some time for our cleanup operations in the
          -- installHandler to do their thing
          threadDelay 300000
          exitFailure
