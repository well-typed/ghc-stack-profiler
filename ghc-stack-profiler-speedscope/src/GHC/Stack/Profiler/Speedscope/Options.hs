module GHC.Stack.Profiler.Speedscope.Options (
  Opts (..),
  AggregationMode (..),
  OutputFile,
  EventlogSource,
  IpeDBOpts (..),
  opts,
  withEventlogSource,
  withOutputFile,
) where

import Control.Applicative (asum)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified IpeDB.Database as DB
import qualified Options.Applicative as O
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
import qualified System.IO as IO

data Opts = Opts
  { eventlogSource :: EventlogSource
  , outputFile :: OutputFile
  , isolateStart :: Maybe Text
  , isolateEnd :: Maybe Text
  , aggregationMode :: AggregationMode
  , maybeIpeDBOpts :: Maybe IpeDBOpts
  }

data AggregationMode
  = PerThread
  | PerCapability
  | NoAggregation

opts :: O.ParserInfo Opts
opts =
  O.info
    (optsParser O.<**> O.helper)
    ( O.fullDesc
        <> O.progDesc "Generate a speedscope.app json file from an eventlog"
        <> O.header "hs-speedscope"
    )

optsParser :: O.Parser Opts
optsParser =
  Opts
    <$> eventlogSourceParser
    <*> outputFileParser
    <*> O.optional isolateStartParser
    <*> O.optional isolateEndParser
    <*> aggregationModeParser
    <*> O.optional ipeDBOptsParser

--------------------------------------------------------------------------------
-- Output file

data OutputFile
  = OutputToStdout
  | OutputToFile !(Maybe FilePath)

instance IsString OutputFile where
  fromString :: String -> OutputFile
  fromString x = if x == "-" then OutputToStdout else OutputToFile (Just x)

outputFileParser :: O.Parser OutputFile
outputFileParser =
  asum
    [ O.strOption . mconcat $
        [ O.metavar "FILE"
        , O.completer (O.bashCompleter "file")
        ]
    , pure $ OutputToFile Nothing
    ]

withOutputFile :: EventlogSource -> OutputFile -> (IO.Handle -> IO a) -> IO a
withOutputFile _eventlogSource (OutputToFile (Just outputPath)) action =
  IO.withFile outputPath IO.WriteMode action
withOutputFile (EventlogFromFile eventlogFile) (OutputToFile Nothing) action =
  IO.withFile (eventlogFile -<.> "json") IO.WriteMode action
withOutputFile _eventlogSource OutputToStdout action =
  action IO.stdout
withOutputFile EventlogFromStdin (OutputToFile Nothing) action =
  action IO.stdout

--------------------------------------------------------------------------------
-- EventlogSource

data EventlogSource
  = EventlogFromStdin
  | EventlogFromFile !FilePath

instance IsString EventlogSource where
  fromString :: String -> EventlogSource
  fromString x = if x == "-" then EventlogFromStdin else EventlogFromFile x

eventlogSourceParser :: O.Parser EventlogSource
eventlogSourceParser =
  O.strArgument . mconcat $
    [ O.metavar "EVENTLOG"
    , O.completer (O.bashCompleter "file")
    ]

withEventlogSource :: EventlogSource -> (IO.Handle -> IO a) -> IO a
withEventlogSource EventlogFromStdin action = do
  stdinIsTerminalDevice <- IO.hIsTerminalDevice IO.stdin
  if stdinIsTerminalDevice
    then do
      putStrLn "Cannot read eventlog from terminal"
      exitFailure
    else do
      IO.hSetBinaryMode IO.stdin True
      action IO.stdin
withEventlogSource (EventlogFromFile eventlogPath) action =
  IO.withBinaryFile eventlogPath IO.ReadMode action

--------------------------------------------------------------------------------
-- Start, End

isolateStartParser :: O.Parser Text
isolateStartParser =
  O.strOption
    ( O.short 's'
        <> O.long "start"
        <> O.metavar "STRING"
        <> O.help "No samples before the first eventlog message with this prefix will be included in the output"
    )

isolateEndParser :: O.Parser Text
isolateEndParser =
  O.strOption
    ( O.short 'e'
        <> O.long "end"
        <> O.metavar "STRING"
        <> O.help "No samples after the first eventlog message with this prefix will be included in the output"
    )

--------------------------------------------------------------------------------
-- Aggregation Mode

aggregationModeParser :: O.Parser AggregationMode
aggregationModeParser =
  asum
    [ O.flag' PerThread (O.long "per-thread" <> O.help "Group the profile per thread (default)")
    , O.flag' PerCapability (O.long "per-capability" <> O.help "Group the profile per capability")
    , O.flag' NoAggregation (O.long "no-aggregation" <> O.help "Perform no grouping, single view")
    , pure PerThread
    ]

--------------------------------------------------------------------------------
-- IPE Database

data IpeDBOpts = IpeDBOpts
  { ipeDBPath :: !FilePath
  , ipeDBFormat :: !DB.TableFormat
  }

ipeDBOptsParser :: O.Parser IpeDBOpts
ipeDBOptsParser =
  IpeDBOpts <$> ipeDBParser <*> (fromMaybe def <$> O.optional ipeDBFormatParser)

ipeDBParser :: O.Parser FilePath
ipeDBParser =
  O.strOption
    ( O.short 'f'
        <> O.long "ipedb"
        <> O.metavar "FILE"
        <> O.help "The path to the IpeDB database."
    )

ipeDBFormatParser :: O.Parser DB.TableFormat
ipeDBFormatParser =
  O.option (O.eitherReader readIpeDBFormat) . mconcat $
    [ O.short 't'
    , O.long "table-format"
    , O.metavar "FORMAT"
    , O.completeWith ["lsm", "tar", "tgz"]
    , O.help "The IpeDB table format (lsm, tar, tgz)."
    ]

readIpeDBFormat :: String -> Either String DB.TableFormat
readIpeDBFormat = \case
  "lsm" -> Right DB.LSMTreeSnapshotV2
  "tar" -> Right DB.LSMTreeSnapshotV2Tar
  "tgz" -> Right DB.LSMTreeSnapshotV2TarGz
  tableFormatString -> Left $! "Unknown table format " <> tableFormatString
