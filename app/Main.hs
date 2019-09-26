{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Lib

import           Data.Version                   ( showVersion )
import           Development.GitRev             ( gitHash
                                                , gitCommitDate
                                                )
import           Options.Applicative
import           Paths_flac2opus                ( version ) -- Magic module that gets the version from the cabal file
import           Protolude               hiding ( option )

opts :: ParserInfo Opts
opts = info
  (helper <*> versionOption <*> optsParser)
  (fullDesc <> progDesc "Convert flac music files to opus." <> header
    "flac2opus - music converter"
  )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> option
          auto
          (  long "workers"
          <> short 'w'
          <> help "Number of workers to spawn"
          <> showDefault
          <> value 1
          <> metavar "INT"
          )
    <*> switch (long "verbose" <> short 'v' <> help "Display verbose output")
    <*> (SrcFilePath <$> strArgument (metavar "SRC"))
    <*> (DstFilePath <$> strArgument (metavar "DST"))

-- | Version option that gets the version from the cabal file and the
-- git hash.
versionOption :: Parser (a -> a)
versionOption = infoOption
  (concat
    [ "riff "
    , showVersion version
    , ", Git revision "
    , $(gitHash)
    , " ("
    , $(gitCommitDate)
    , ")"
    ]
  ) -- TemplateHaskell required for gitrev functions.
  (long "version" <> help "Show version")

main :: IO ()
main = execParser opts >>= run
