{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.Stack.Profiler.Speedscope where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Machine (Is, Moore (..), PlanT, ProcessT, await, construct, source, yield, (~>))
import Data.Machine.Runner (foldlT)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Tuple
import Data.Version
import Data.Word
import GHC.RTS.Events hiding (StringId, header, str)
import qualified Options.Applicative as Options
import Speedscope.Schema
import qualified System.IO.Unsafe as Unsafe
import Text.ParserCombinators.ReadP hiding (between)

import GHC.Stack.Profiler.Core.Eventlog
import GHC.Stack.Profiler.Core.SymbolTable
import GHC.Stack.Profiler.Core.ThreadSample as ThreadSample
import GHC.Stack.Profiler.Core.Util
import GHC.Stack.Profiler.Speedscope.IpeDb (toSpeedscopeInfoProv)
import GHC.Stack.Profiler.Speedscope.Options
import qualified GHC.Stack.Profiler.Speedscope.Options as IpeDbConf (IpeConf (..))
import GHC.Stack.Profiler.Speedscope.Types
import qualified IpeDb.InfoProv as IpeDb
import qualified IpeDb.Query as IpeDb
import qualified IpeDb.Types as IpeDb
import Data.Either (partitionEithers)
import System.IO

entry :: IO ()
entry = do
  os <- Options.execParser options
  run os

run :: SSOptions -> IO ()
run SSOptions{ file, isolateStart, isolateEnd, aggregationMode, ipeDbConf } = withOptionalIpeDb ipeDbConf $ \mInfoProvDb -> do
  -- We extract the 'InfoProv's before collecting the thread samples to avoid
  -- having to sort the 'InfoProv's and causing a space leak.
  -- We reduce the memory usage considerably by doing this separately.
  oracle <- case mInfoProvDb of
    Just (conf, db) -> do
      case index conf of
        True -> IpeDb.populateFromEventlog db file
        False -> pure ()
      pure $ oracleFromInfoProvDb db
    Nothing -> do
      elIpe <- either error id <$> readEventLogFromFile file
      let infoProv = parseIpeTraces (isolateStart, isolateEnd) elIpe
      pure $ oracleFromMap infoProv

  -- Now do the actual work
  el <- either error id <$> readEventLogFromFile file
  profileJson <- convertToSpeedscope aggregationMode (isolateStart, isolateEnd) isInfoEvent processEventsDefault oracle el
  encodeFile (file ++ ".json") profileJson

withOptionalIpeDb :: Maybe IpeConf -> (Maybe (IpeConf, IpeDb.InfoProvDb) -> IO a) -> IO ()
withOptionalIpeDb mConf act = case mConf of
  Nothing -> do
    _ <- act Nothing
    pure ()
  Just conf -> IpeDb.withInfoProvDb (IpeDbConf.file conf) $ \ db -> do
    _ <- act (Just (conf, db))
    pure ()

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

processEventLogProfileState :: Aggregation -> EventLogProfileState -> EventLogProfile
processEventLogProfileState mode evProfileState =
  EventLogProfile
    { profileProgName = profile_name
    , profileFrames = all_frames
    , profileProfiles = map (mkProfile profile_name interval) stack_profile_samples
    , profileProgVersionString = Text.pack $ "ghc-stack-profiler-speedscope@" ++ CURRENT_PACKAGE_VERSION
    , profileProcessingErrors = decoding_errors evProfileState
    }
  where
    profile_name = fromMaybe "" (eventlog_prog_name evProfileState)

    interval = fromMaybe 1 (prof_interval evProfileState)

    all_frames :: [Frame]
    all_frames =
      map snd . sortOn fst $
        map (fmap mkCostCentreFrame) (Map.elems $ user_cost_centres evProfileState)

    stack_profile_samples :: [(Word64, [[Int]])]
    stack_profile_samples =
      case mode of
        PerThread ->
          -- groupSort is assumed to be stable
          groupSort $ map mkThreadSample (reverse $ el_samples evProfileState)
        PerCapability ->
          -- groupSort is assumed to be stable
          groupSort $ map mkCapabilitySample (reverse $ el_samples evProfileState)
        NoAggregation ->
          [(1, map mkSingleProfileSample (reverse $ el_samples evProfileState))]

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
  -> (EventLogProfileState -> Event -> EventLogProfileState)
  -- ^ Specifies how to build the profile given the events included based on the
  -- delimiters and predicate
  -> InfoProvOracle
  -- ^ Already gathered table for 'InfoProv's that have been collected prior.
  -> EventLog
  -> IO Value
convertToSpeedscope mode (is, ie) considerEvent processEvents infoProvOracle (EventLog _h (Data es)) = do
  eventLogProfileState <-
    foldlT processEvents initEL $
        source es ~>
        delimit considerEvent (markers (is, ie))
  let eventlogProfile = processEventLogProfileState mode eventLogProfileState
  logDecodingErrors eventlogProfile
  pure $ toJSON File
    { shared             = Shared{ frames = profileFrames eventlogProfile }
    , profiles           = profileProfiles eventlogProfile
    , name               = Just $ profileProgName eventlogProfile
    , activeProfileIndex = Just 0
    , exporter           = Just $ profileProgVersionString eventlogProfile
    }
  where
    initEL = EventLogProfileState
      { eventlog_prog_name = Nothing
      , rts_version = Nothing
      , prof_interval = Nothing
      , info_prov_oracle = infoProvOracle
      , user_cost_centres = Map.empty
      , cost_centre_counter = 0
      , el_samples = []
      , hydration_table = emptyIntMapTable
      , current_callstack_chunks = []
      , decoding_errors = []
      }

    logDecodingErrors prof = case profileProcessingErrors prof of
      [] -> pure ()
      errorStacks -> do
        hPutStrLn stderr $ show (length errorStacks) ++ " CallStacks were decoded incompletely or not at all."
        forM_ errorStacks $ \ stack -> do
          let
            size = length stack

          forM_ (take 10 stack) $ \ err -> do
            hPutStrLn stderr ("  " ++ prettyEventLogError err)

          when (size > 10) $ do
            hPutStrLn stderr ("  ... and " ++ show (size - 10) ++ " more." )

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

processIpeEventsDefault :: InfoProvMap -> Event -> InfoProvMap
processIpeEventsDefault provMap (Event _t ei _c) =
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
        IntMap.insert (word64ToInt $ coerce key) prov provMap
    _ ->
      provMap

-- | Default processing function to convert profiling events into a classic speedscope
-- profile
processEventsDefault :: EventLogProfileState -> Event -> EventLogProfileState
processEventsDefault elProf (Event _t ei _c) =
  case ei of
    ProgramArgs _ (pname: _args) ->
      elProf { eventlog_prog_name = Just pname }
    RtsIdentifier _ rts_ident ->
      elProf { rts_version = parseIdent rts_ident }
    ProfBegin ival ->
      elProf { prof_interval = Just ival }
    UserBinaryMessage bs ->
      case deserializeEventlogMessage $ LBS.fromStrict bs of
        Left _err ->
          elProf
        Right evMsg -> case evMsg of
          CallStackFinal msg ->
            let
              (callStackMessage, elProf1) =
                hydrateBinaryEventlog elProf msg
            in
              addCallStackToProfile elProf1 callStackMessage

          CallStackChunk msg ->
            elProf { current_callstack_chunks = msg : current_callstack_chunks elProf }
          StringDef msg ->
            elProf { hydration_table = insertTextMessage msg (hydration_table elProf) }
          SourceLocationDef msg ->
            case insertSourceLocationMessage msg (hydration_table elProf) of
              Left err ->
                addDecodingErrorsForStack [fromMissingKeyError err] elProf

              Right newTable ->
                elProf { hydration_table = newTable }
    _ ->
      elProf

addCallStackToProfile :: EventLogProfileState -> CallStackMessage -> EventLogProfileState
addCallStackToProfile elProf MkCallStackMessage{callThreadId, callCapabilityId, callStack} =
  let
    (newProf, ccsOrError) = mapAccumR go elProf callStack
    (errors, ccs) = partitionEithers ccsOrError
    go prof csi =
      swap $ lookupOrAddStackItemToProfile prof csi
    sample = Sample
      { sampleThreadId = callThreadId
      , sampleCapabilityId = callCapabilityId
      , sampleCostCentreStack =
          -- TODO: Don't do the arbitrary cutoff, but reduce cycles.
          -- And then do an arbitrary cutoff as speedscope has limits.
          take 1000 $ reverse ccs
      }
  in
    addDecodingErrorsForStack
      errors
      newProf
        { el_samples = sample : el_samples newProf
        }

hydrateBinaryEventlog :: EventLogProfileState -> BinaryCallStackMessage -> (CallStackMessage, EventLogProfileState)
hydrateBinaryEventlog elProf msg =
  let
    chunks = current_callstack_chunks elProf
    -- Why reverse?
    -- When decoding the stack, we walk the stack from the top down.
    -- Afterwards, the stack is chunked to fit into a single eventlog line,
    -- and the chunks are written in ascending order to the eventlog.
    -- When we pick up these messages one after another, they are prepended to
    -- 'current_callstack_chunks', thus we are essentially storing the chunks in reverse
    -- order, as the first chunk we encounter is the top of the stack, etc...
    --
    -- Concrete example, assuming a stack @[1,2,3,4,5,6]@ and chunk size of 2:
    --
    -- 1. Chunk it: @[1,2] [3,4] [5,6]@
    -- 2. Write it to the eventlog in this order, so the messages are:
    --    [1,2]
    --    [3,4]
    --    [5,6]
    -- 3. When reading the eventlog, we store prepend later messages, resulting in:
    --    [5,6] [3,4] [1,2]
    -- 4. One reverse later: @[1,2] [3,4] [5,6]@
    -- 5. Now we can finally concat the stack frame chunks.
    orderedChunks = NonEmpty.reverse $ msg :| chunks
    fullBinaryCallStackMessage = catCallStackMessage orderedChunks
    (callStackMessage, errs) =
      hydrateEventlogCallStackMessage
        (mkIntMapSymbolTableReader (hydration_table elProf))
        fullBinaryCallStackMessage
  in
    ( callStackMessage
    , addDecodingErrorsForStack
        (map fromBinaryError errs)
        elProf { current_callstack_chunks = [] }
    )

lookupOrAddStackItemToProfile :: EventLogProfileState -> StackItem -> (Either EventLogError CostCentreId, EventLogProfileState)
lookupOrAddStackItemToProfile elProf = \ case
  IpeId iid -> do
    case findInfoProv (info_prov_oracle elProf) iid of
      Nothing ->
        (Left $ UnknownIpeId iid, elProf)
      Just prov ->
        lookupOrInsertCostCentre (CostCentreIpe prov)

  ThreadSample.UserMessage msg ->
    lookupOrInsertCostCentre (CostCentreMessage $ fromString msg)

  SourceLocation loc ->
    lookupOrInsertCostCentre (CostCentreSrcLoc loc)

  where
    lookupOrInsertCostCentre userMessage =
      case Map.lookup userMessage (user_cost_centres elProf) of
        Just (cid, _) -> (Right cid, elProf)
        Nothing ->
          let
            key = userMessage
            cid = cost_centre_counter elProf
          in
            ( Right cid
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

data EventLogProfile = EventLogProfile
  { profileProgName :: Text
  , profileFrames :: [Frame]
  , profileProfiles :: [Profile]
  , profileProgVersionString :: Text
  , profileProcessingErrors :: [[EventLogError]]
  }

-- | The type we wish to convert event logs into
data EventLogProfileState = EventLogProfileState
  { eventlog_prog_name :: Maybe Text
  , rts_version :: Maybe (Version, Text)
  , prof_interval :: Maybe Word64
  , info_prov_oracle :: InfoProvOracle
  -- ^ Table of present 'InfoProv's in the eventlog
  , user_cost_centres :: !(Map UserCostCentre (CostCentreId, UserCostCentre))
  -- ^ All "cost centres" that are actually mentioned by `ghc-stack-profiler`.
  , cost_centre_counter :: !CostCentreId
  -- ^ Unique Counter for the 'Sample's
  , el_samples :: [Sample]
  -- ^ All samples in the reverse order of finding them in the eventlog.
  , hydration_table :: !IntMapTable
  -- ^ The symbol table storing 'Text' and 'SourceLocation' symbols
  -- for hydrating a 'BinaryCallStackMessage' into a 'CallStackMessage'.
  , current_callstack_chunks :: [BinaryCallStackMessage]
  -- ^ Chunks of 'BinaryCallStackMessage' we are currently decoding.
  -- All chunks are assumed to be from the same callstack and will be decoded once a
  -- 'CallStackFinal' message is encountered.
  , decoding_errors :: [[EventLogError]]
  }

addDecodingErrorsForStack :: [EventLogError] -> EventLogProfileState -> EventLogProfileState
addDecodingErrorsForStack errs elProf =
  if null errs
    then
      elProf
    else
      elProf
        { decoding_errors = errs : decoding_errors elProf
        }

-- ----------------------------------------------------------------------------
-- Decoding errors
-- ----------------------------------------------------------------------------

data EventLogError
  = UnknownIpeId IpeId
  | UnknownStringId StringId
  | UnknownSrcLocId SourceLocationId
  | SourceLocationPartUndefined SourceLocationId StringId
  deriving (Show, Eq, Ord)

fromBinaryError :: BinaryCallStackDecodeError -> EventLogError
fromBinaryError = \ case
  StringIdNotFound sid -> UnknownStringId sid
  SourceLocationIdNotFound sid -> UnknownSrcLocId sid

fromMissingKeyError :: MissingKeyError -> EventLogError
fromMissingKeyError = \ case
  KeyStringIdNotFound sourceLocId stringId -> SourceLocationPartUndefined sourceLocId stringId

prettyEventLogError :: EventLogError -> String
prettyEventLogError = \ case
  UnknownIpeId ipeId -> show $ UnknownIpeId ipeId
  UnknownStringId sid -> show $ UnknownStringId sid
  UnknownSrcLocId sid -> show $ UnknownSrcLocId sid
  SourceLocationPartUndefined srcLocId stringId ->
    "While decoding source location " ++ show srcLocId ++ ", failed to find string " ++ show stringId

-- ----------------------------------------------------------------------------
-- Oracle for Info Provs
-- ----------------------------------------------------------------------------

newtype InfoProvOracle = InfoProvOracle
  { findInfoProv :: IpeId -> Maybe InfoProv
  }

oracleFromMap :: IntMap InfoProv -> InfoProvOracle
oracleFromMap infoProvs = InfoProvOracle (\ipeId -> IntMap.lookup (idToInt ipeId) infoProvs)

oracleFromInfoProvDb :: IpeDb.InfoProvDb -> InfoProvOracle
oracleFromInfoProvDb infoProvDb = InfoProvOracle $ \ipeId -> do
  ipedb_info_prov <- Unsafe.unsafePerformIO (IpeDb.lookupInfoProv infoProvDb (IpeDb.IpeId (getIpeId ipeId)))
  pure $ toSpeedscopeInfoProv ipedb_info_prov

-- ----------------------------------------------------------------------------
-- In-memory Info Prov DB
-- ----------------------------------------------------------------------------

type InfoProvMap = IntMap InfoProv

parseIpeTraces
  :: (Maybe Text, Maybe Text)
  -> EventLog
  -> IntMap InfoProv
parseIpeTraces (is, ie) (EventLog _h (Data es)) =
  info_provs
  where
    Identity info_provs =
        foldlT processIpeEventsDefault initEL $
            source es ~>
            delimit isIpeInfoEvent (markers (is, ie))

    initEL = IntMap.empty
