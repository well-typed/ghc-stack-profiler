module GHC.Stack.Profiler.Speedscope.Options where

import Data.Text (Text)
import Options.Applicative

data SSOptions = SSOptions
  { file :: FilePath
  , isolateStart :: Maybe Text
  , isolateEnd :: Maybe Text
  , aggregationMode :: Aggregation
  , ipeDbConf :: Maybe IpeConf
  }
  deriving (Show)

data IpeConf = IpeConf
  { file :: FilePath
  , index :: Bool
  }
  deriving (Show)

data Aggregation
  = PerThread
  | PerCapability
  | NoAggregation
  deriving (Show, Eq, Ord)

options :: ParserInfo SSOptions
options =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc "Generate a speedscope.app json file from an eventlog"
        <> header "hs-speedscope"
    )

optsParser :: Parser SSOptions
optsParser =
  SSOptions
    <$> argument str (metavar "FILE.eventlog")
    <*> optional
      ( strOption
          ( short 's'
              <> long "start"
              <> metavar "STRING"
              <> help "No samples before the first eventlog message with this prefix will be included in the output"
          )
      )
    <*> optional
      ( strOption
          (short 'e' <> long "end" <> metavar "STRING" <> help "No samples after the first eventlog message with this prefix will be included in the output")
      )
    <*> ( flag' PerThread (long "per-thread" <> help "Group the profile per thread (default)")
            <|> flag' PerCapability (long "per-capability" <> help "Group the profile per capability")
            <|> flag' NoAggregation (long "no-aggregation" <> help "Perform no grouping, single view")
            <|> pure PerThread
        )
    <*> optional ipeConfParser

ipeConfParser :: Parser IpeConf
ipeConfParser =
  IpeConf
    <$> strOption (short 'f' <> long "ipedb" <> metavar "FILE" <> help "'ipedb' database location")
    <*> flag False True (long "index" <> help "Create an index on-the-fly at the database location instead of in-memory database. Will overwrite any existing database.")
