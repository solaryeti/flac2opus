module Main where

import Protolude hiding (option)
import Lib
import Options.Applicative

opts :: ParserInfo Opts
opts =
    info (helper <*> optsParser)
           ( fullDesc
          <> progDesc "Convert flac music files to opus."
          <> header "flac2opus - music converter" )

optsParser :: Parser Opts
optsParser = Opts
      <$> option auto
          ( long "workers"
         <> short 'w'
         <> help "Number of workers to spawn"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Display verbose output" )
      <*> fmap SrcFilePath (strArgument (metavar "SRC"))
      <*> fmap DstFilePath (strArgument (metavar "DST"))

main :: IO ()
main = execParser opts >>= run
