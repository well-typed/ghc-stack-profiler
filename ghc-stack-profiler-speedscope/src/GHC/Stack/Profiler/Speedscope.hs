{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Stack.Profiler.Speedscope where

import Data.String ( fromString )
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Tuple
import Data.Functor.Identity (Identity (..))
import qualified Data.ByteString.Lazy as LBS
import Data.List.Extra
import Data.Machine (Moore (..), source, (~>), ProcessT, PlanT, Is, construct, await, yield)
import Data.Machine.Runner (foldlT)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Text (Text)
import Data.Version
import Data.Word
import GHC.RTS.Events hiding (header, str)
import qualified Options.Applicative as O
import Options.Applicative hiding (optional)
import Speedscope.Schema
import Text.ParserCombinators.ReadP hiding (between)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Coerce (coerce)

import GHC.Stack.Profiler.ThreadSample as ThreadSample
import GHC.Stack.Profiler.Util

data SSOptions = SSOptions
  { file :: FilePath
  , isolateStart :: Maybe Text
  , isolateEnd :: Maybe Text
  , aggregationMode :: Aggregation
  } deriving Show

data Aggregation
  = PerThread
  | PerCapability
  | NoAggregation
  deriving (Show, Eq, Ord)

optsParser :: Parser SSOptions
optsParser = SSOptions
  <$> argument str (metavar "FILE.eventlog")
  <*> O.optional (strOption
    ( short 's'
    <> long "start"
    <> metavar "STRING"
    <> help "No samples before the first eventlog message with this prefix will be included in the output" ))
  <*> O.optional (strOption
    ( short 'e' <> long "end" <> metavar "STRING" <> help "No samples after the first eventlog message with this prefix will be included in the output" ))
  <*>
    (   flag' PerThread     (long "per-thread"     <> help "Group the profile per thread (default)")
    <|> flag' PerCapability (long "per-capability" <> help "Group the profile per capability")
    <|> flag' NoAggregation (long "no-aggregation" <> help "Perform no grouping, single view")
    <|> pure  PerThread
    )

entry :: IO ()
entry = do
  os <- execParser opts
  run os
  where
    opts = info (optsParser <**> helper)
      ( fullDesc
     <> progDesc "Generate a speedscope.app json file from an eventlog"
     <> header "hs-speedscope" )

run :: SSOptions -> IO ()
run SSOptions{ file, isolateStart, isolateEnd, aggregationMode } = do
  -- We extract the 'InfoProv's before collecting the thread samples to avoid
  -- having to sort the 'InfoProv's and causing a memory leak.
  -- We reduce the memory usage considerably by doing this separately.
  elIpe <- either error id <$> readEventLogFromFile file
  let infoProv = parseIpeTraces  (isolateStart, isolateEnd) elIpe

  -- Now do the actual work
  el <- either error id <$> readEventLogFromFile file
  encodeFile (file ++ ".json") (convertToSpeedscope aggregationMode (isolateStart, isolateEnd) isInfoEvent processEventsDefault infoProv el)

-- | A Moore machine whose state indicates which delimiting markers have been
-- seen. If both markers are 'Nothing', then the state will always be 'True'.
-- If the first marker is given, then the state will always be 'False' until a
-- value which the marker is a prefix of is seen. If the second marker is given,
-- then the state will always be 'False' after a value which the marker is a
-- prefix of is seen.
markers :: (Maybe Text, Maybe Text) -> Moore Text Bool
markers (Nothing, Nothing) =
    go
  where
    go = Moore True (const go)
markers (Just s,  Nothing) =
    wait_for_start
  where
    wait_for_start =
      Moore False $ \s' ->
        if s `Text.isPrefixOf` s' then
          go
        else
          wait_for_start
    go = Moore True (const go)
markers (Nothing, Just e) =
    go_until
  where
    go_until =
      Moore True $ \e' ->
        if e `Text.isPrefixOf` e' then
          stop
        else
          go_until
    stop = Moore False (const stop)
markers (Just s, Just e) =
    go_between
  where
    go_between =
        Moore False wait_for_start
      where
        wait_for_start s' = if s `Text.isPrefixOf` s' then go_until else go_between
    go_until =
        Moore True close'
      where
        close' e' = if e `Text.isPrefixOf` e' then stop else go_until
    stop = Moore False (const stop)

-- | Delimit the event process, and only include events which satisfy a
-- predicate.
delimit
    :: Monad m
    => (EventInfo -> Bool)
    -- ^ Only emit events which pass this predicate
    -> Moore Text Bool
    -- ^ Only emit events when this 'Moore' process state is 'True'. The process
    -- will be given the values of 'UserMarker's in the event log.
    -> ProcessT m Event Event
delimit p =
    construct . go
  where
    go :: Monad m => Moore Text Bool -> PlanT (Is Event) Event m ()
    go mm@(Moore s next) = do
      e <- await
      case evSpec e of
        -- on marker step the moore machine.
        UserMarker m -> do
            let mm'@(Moore s' _) = next m
            -- if current or next state is open (== True), emit the marker.
            when (s || s') $ yield e
            go mm'

        -- for other events, emit if the state is open and predicate passes
        ei -> do
            when (s || p ei) $ yield e
            go mm

parseIpeTraces
  :: (Maybe Text, Maybe Text)
  -> EventLog
  -> IntMap InfoProv
parseIpeTraces (is, ie) (EventLog _h (Data es)) =
  infoProvs
  where
    Identity (EventLogProfile _ _ _ infoProvs _userCostCentres _counter _samples) =
        foldlT processIpeEventsDefault initEL $
            source es ~>
            delimit isIpeInfoEvent (markers (is, ie))

    initEL = EventLogProfile
      { prog_name = Nothing
      , rts_version = Nothing
      , prof_interval = Nothing
      , info_provs = IntMap.empty
      , user_cost_centres = Map.empty
      , cost_centre_counter = 0
      , el_samples = []
      }

-- | Convert an 'EventLog' into a speedscope profile JSON value. To convert the
-- event log using the traditional profile extraction logic, use `isInfoEvent`
-- and `processEventsDefault` for the predicate and processing function,
-- respectively.
convertToSpeedscope
  :: Aggregation
  -- ^ How to aggregate the stack profile samples
  -> (Maybe Text, Maybe Text)
  -- ^ Delimiting markers. No events before a user marker containing the first
  -- string will be included. No events after a user marker containing the
  -- second string will be included.
  -> (EventInfo -> Bool)
  -- ^ Only consider events which satisfy this predicate
  -> (EventLogProfile -> Event -> EventLogProfile)
  -- ^ Specifies how to build the profile given the events included based on the
  -- delimiters and predicate
  -> IntMap InfoProv
  -- ^ Already gathered table for 'InfoProv's that have been collected prior.
  -> EventLog
  -> Value
convertToSpeedscope mode (is, ie) considerEvent processEvents infoProvs (EventLog _h (Data es)) =
  case el_version of
    Just (ghc_version, _) | ghc_version < makeVersion [8,9,0]  ->
      error ("Eventlog is from ghc-" ++ showVersion ghc_version ++ " hs-speedscope only works with GHC 8.10 or later")
    _ -> toJSON file
      where
        file = File
          { shared             = Shared{ frames = all_frames }
          , profiles           = map (mkProfile profile_name interval) stack_profile_samples
          , name               = Just profile_name
          , activeProfileIndex = Just 0
          , exporter           = Just $ fromString version_string
          }
  where
    Identity (EventLogProfile (fromMaybe "" -> profile_name) el_version (fromMaybe 1 -> interval) _infoProvs userCostCentres _counter samples) =
        foldlT processEvents initEL $
            source es ~>
            delimit considerEvent (markers (is, ie))

    initEL = EventLogProfile
      { prog_name = Nothing
      , rts_version = Nothing
      , prof_interval = Nothing
      , info_provs = infoProvs
      , user_cost_centres = Map.empty
      , cost_centre_counter = 0
      , el_samples = []
      }

    version_string :: String
    version_string = "hs-speedscope@" ++ CURRENT_PACKAGE_VERSION

    all_frames :: [Frame]
    all_frames =
      map snd . sortOn fst $
        map (fmap mkCostCentreFrame) (Map.elems userCostCentres)

    stack_profile_samples :: [(Word64, [[Int]])]
    stack_profile_samples =
      case mode of
        PerThread ->
          -- groupSort is assumed to be stable
          groupSort $ map mkThreadSample (reverse samples)
        PerCapability ->
          -- groupSort is assumed to be stable
          groupSort $ map mkCapabilitySample (reverse samples)
        NoAggregation ->
          [(1, map mkSingleProfileSample (reverse samples))]

    mkCostCentreFrame :: UserCostCentre -> Frame
    mkCostCentreFrame = \ case
      CostCentreMessage msg ->
        Frame
          { name = msg
          , file = Nothing
          , col = Nothing
          , line = Nothing
          }
      CostCentreSrcLoc loc ->
        Frame
          { name = functionName loc
          , file = Just $ fileName loc
          , col = Just $ word32ToInt $  ThreadSample.column loc
          , line = Just $ word32ToInt $ ThreadSample.line loc
          }
      CostCentreIpe InfoProv{infoTableName, infoProvModule, infoProvLabel, infoProvSrcLoc} ->
        Frame
          { name =
              if Text.null infoProvLabel
                then infoProvModule <> " " <> infoTableName
                else infoProvModule <> " " <> infoProvLabel
          , file = if infoProvSrcLoc == ":" then Just infoProvModule else  fileName
          , col = columnM
          , line = lineM
          }
        where
          (fileName, lineM, columnM) = tryParsingGhcSrcSpan infoProvSrcLoc

    mkThreadSample :: Sample -> (Word64, [Int])
    mkThreadSample (Sample ti _capId ccs) = (ti, map (word64ToInt . coerce) ccs)

    mkCapabilitySample :: Sample -> (Word64, [Int])
    mkCapabilitySample (Sample _ti capId ccs) = (coerce capId, map (word64ToInt . coerce) ccs)

    mkSingleProfileSample :: Sample -> [Int]
    mkSingleProfileSample (Sample _ti _capId ccs) = map (word64ToInt . coerce) ccs

-- TODO: parsing of various src span formats
-- currently supported:
-- * filename.hs:1:1
-- * filename.hs:1-4:1
-- * filename.hs:1:1-4
-- * filename.hs:1-4:1-4 (this one doesn't mean anything)
--
-- missing:
-- * filename.hs:(154,1)-(155,32)
tryParsingGhcSrcSpan :: Text -> (Maybe Text, Maybe Int, Maybe Int)
tryParsingGhcSrcSpan srcSpan =
  case Text.splitOn ":" srcSpan of
    (fn:srcLoc) -> case srcLoc of
      (rowSpan: columnSpan:_) ->
        let
          getStartOfSpan sp = case Text.splitOn "-" sp of
            start:_ -> readAsNumber start
            _ -> Nothing
        in
          (Just fn, getStartOfSpan rowSpan, getStartOfSpan columnSpan)
      _ ->
          (Just fn, Nothing, Nothing)
    _ ->
      (Nothing, Nothing, Nothing)
  where
    readAsNumber n = case Read.decimal n of
      Left _ -> Nothing
      Right (number, _) -> Just number

processIpeEventsDefault :: EventLogProfile -> Event -> EventLogProfile
processIpeEventsDefault elProf (Event _t ei _c) =
  case ei of
    InfoTableProv { itTableName, itClosureDesc, itTyDesc, itInfo, itLabel, itSrcLoc, itModule } ->
      let
        key = InfoProvId itInfo
        prov = InfoProv
          { infoProvId = InfoProvId itInfo
          , infoProvSrcLoc = itSrcLoc
          , infoProvModule = itModule
          , infoProvLabel = itLabel
          , infoTableName = itTableName
          , infoClosureDesc = itClosureDesc
          , infoTyDesc = itTyDesc
          }
      in
        elProf
          { info_provs = IntMap.insert (word64ToInt $ coerce key) prov (info_provs elProf)
          }
    _ ->
      elProf

-- | Default processing function to convert profiling events into a classic speedscope
-- profile
processEventsDefault :: EventLogProfile -> Event -> EventLogProfile
processEventsDefault elProf (Event _t ei _c) =
  case ei of
    ProgramArgs _ (pname: _args) ->
      elProf { prog_name = Just pname }
    RtsIdentifier _ rts_ident ->
      elProf { rts_version = parseIdent rts_ident }
    ProfBegin ival ->
      elProf { prof_interval = Just ival }
    UserBinaryMessage bs ->
      case deserializeCallStackMessage $ LBS.fromStrict bs of
        Left _err ->
          elProf
        Right csMsg ->
          addCallStackToProfile elProf csMsg
    _ ->
      elProf

addCallStackToProfile :: EventLogProfile -> CallStackMessage -> EventLogProfile
addCallStackToProfile elProf MkCallStackMessage{callThreadId, callCapabilityId, callStack} =
  let
    (newProf, ccs) = mapAccumR go elProf callStack
    go prof csi =
      swap $ lookupOrAddStackItemToProfile prof csi
    sample = Sample
      { sampleThreadId = callThreadId
      , sampleCapabilityId = callCapabilityId
      , sampleCostCentreStack = reverse ccs
      }
  in
    newProf
      { el_samples = sample : el_samples newProf
      }

lookupOrAddStackItemToProfile :: EventLogProfile -> StackItem -> (CostCentreId, EventLogProfile)
lookupOrAddStackItemToProfile elProf = \ case
  IpeId iid ->
    lookupOrInsertCostCentre (CostCentreIpe (info_provs elProf IntMap.! word64ToInt iid))
  ThreadSample.UserMessage msg ->
    lookupOrInsertCostCentre (CostCentreMessage $ fromString msg)
  SourceLocation loc ->
    lookupOrInsertCostCentre (CostCentreSrcLoc loc)
  where
    lookupOrInsertCostCentre userMessage =
      case Map.lookup userMessage (user_cost_centres elProf) of
        Just (cid, _) -> (cid, elProf)
        Nothing ->
          let
            key = userMessage
            cid = cost_centre_counter elProf
          in
            ( cid
            , elProf
              { cost_centre_counter = cid + 1
              , user_cost_centres = Map.insert key (cid, key) (user_cost_centres elProf)
              }
            )

isIpeInfoEvent :: EventInfo -> Bool
isIpeInfoEvent InfoTableProv {}      = True
isIpeInfoEvent _ = False

isInfoEvent :: EventInfo -> Bool
isInfoEvent ProgramArgs {}        = True
isInfoEvent RtsIdentifier {}      = True
isInfoEvent ProfBegin {}          = True
isInfoEvent UserBinaryMessage {}  = True
isInfoEvent _ = False

mkProfile :: Text -> Word64 -> (Word64, [[Int]]) -> Profile
mkProfile pname interval (n, samples) = SampledProfile sampledProfile
  where
    sampledProfile = MkSampledProfile
      { unit       = Nanoseconds
      , name       = pname <> " " <> Text.show n
      , startValue = 0
      , endValue   = length samples
      , weights    = fromIntegral <$> sample_weights
      , samples
      }
    sample_weights = replicate (length samples) interval

parseIdent :: Text -> Maybe (Version, Text)
parseIdent s = convert $ listToMaybe $ flip readP_to_S (Text.unpack s) $ do
  void $ string "GHC-"
  [v1, v2, v3] <- replicateM 3 (intP <* optional (char '.'))
  skipSpaces
  return $ makeVersion [v1,v2,v3]
  where
    intP = do
      x <- munch1 isDigit
      return $ read x

    convert x = (\(a, b) -> (a, Text.pack b)) <$> x

-- | The type we wish to convert event logs into
data EventLogProfile = EventLogProfile
    { prog_name :: Maybe Text
    , rts_version :: Maybe (Version, Text)
    , prof_interval :: Maybe Word64
    , info_provs :: !(IntMap InfoProv)
    -- ^ Table of present 'InfoProv's in the eventlog
    , user_cost_centres :: !(Map UserCostCentre (CostCentreId, UserCostCentre))
    -- ^ All "cost centres" that are actually mentioned by `ghc-stack-profiler`.
    , cost_centre_counter :: !CostCentreId
    -- ^ Unique Counter for the 'Sample's
    , el_samples :: [Sample]
    -- ^ All samples in the reverse order of finding them in the eventlog.
    }  deriving (Eq, Ord, Show)

newtype CostCentreId = CostCentreId Word64
  deriving (Eq, Ord, Show)
  deriving newtype (Num)

newtype InfoProvId = InfoProvId Word64
  deriving (Eq, Ord, Show)

data UserCostCentre
  = CostCentreMessage !Text
  | CostCentreSrcLoc !SourceLocation
  | CostCentreIpe !InfoProv
  deriving (Eq, Ord, Show)

data InfoProv = InfoProv
  { infoProvId :: !InfoProvId
  , infoProvSrcLoc :: !Text
  , infoProvModule :: !Text
  , infoProvLabel :: !Text
  , infoTableName :: !Text
  , infoClosureDesc :: !Int
  , infoTyDesc :: !Text
  }  deriving (Eq, Ord, Show)

data Sample = Sample
  { sampleThreadId :: !Word64 -- ^ thread id
  , sampleCapabilityId :: !CapabilityId -- ^ Capability id
  , sampleCostCentreStack :: [CostCentreId] -- ^ stack ids
  }
  deriving (Eq, Ord, Show)
