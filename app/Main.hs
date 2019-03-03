module Main where

import Protolude hiding (option)
import Lib
import Options.Applicative

data Opts = Opts
  { workers :: Int
  , src     :: Text
  , dest    :: Text
  } deriving (Show)

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
      <*> strArgument (metavar "SRC")
      <*> strArgument (metavar "DST")

main :: IO ()
main = do
  opts' <- execParser opts
  run (src opts') (dest opts') (workers opts')
