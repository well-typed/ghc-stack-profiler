{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Stack.Profiler.Speedscope (main) where

import Control.Applicative (Alternative (..))
import Control.Monad (replicateM, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as JSON (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List.Extra (groupSort, mapAccumR, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Machine (AutomatonM (..), Is, Moore (..), PlanT, ProcessT, await, construct, final, runT, yield, (~>))
import qualified Data.Machine as M
import Data.Machine.MealyT (scanMealyTM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (Version, makeVersion)
import Data.Word (Word64)
import qualified GHC.RTS.Events as E
import qualified GHC.RTS.Events.Incremental as E
import qualified GHC.Stack.Profiler.Core.Eventlog as GSP
import qualified GHC.Stack.Profiler.Core.SymbolTable as GSP
import qualified GHC.Stack.Profiler.Core.ThreadSample as GSP
import qualified GHC.Stack.Profiler.Core.Util as GSP (word64ToInt)
import GHC.Stack.Profiler.Speedscope.Options
import GHC.Stack.Profiler.Speedscope.Types
import qualified IpeDB.Database as DB
import qualified IpeDB.Types.InfoProv as IP
import qualified Options.Applicative as O
import qualified Speedscope.Schema as Speedscope
import qualified System.IO as IO
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = run =<< O.execParser opts

run :: Opts -> IO ()
run Opts{eventlogSource, outputFile, isolateStart, isolateEnd, aggregationMode, maybeIpeDBOpts} =
  -- Create an IPE table.
  withIpeDB maybeIpeDBOpts $ \infoProvTable -> do
    -- NOTE: The eventlog file is read twice, because in certain cases
    --       `ghc-stack-profiler` events _may_ precede the init events,
    --       which would require that we sort the _entire_ eventlog.
    --       Traversing the file twice means that we only have to sort
    --       the events that contain stack frames.
    --
    -- Index the IPE events.
    when (isNothing maybeIpeDBOpts) $
      withEventlogSource eventlogSource $ \eventlogHandle ->
        M.runT_ $
          fromHandle eventlogHandle
            ~> decodeEvents
            ~> DB.indexer IP.toInfoProv def infoProvTable

    -- Convert the eventlog to a speedscope file.
    let
      isProfileEvent :: E.EventInfo -> Bool
      isProfileEvent E.ProgramArgs{} = True
      isProfileEvent E.RtsIdentifier{} = True
      isProfileEvent E.ProfBegin{} = True
      isProfileEvent E.UserBinaryMessage{} = True
      isProfileEvent _ = False
    eventlogProfile <-
      processEventlogProfile
        aggregationMode
        (isolateStart, isolateEnd)
        isProfileEvent
        infoProvTable
        eventlogSource
    let
      speedscopeFile = toSpeedscopeFile eventlogProfile
    withOutputFile eventlogSource outputFile $ \outputHandle ->
      BSL.hPutStr outputHandle (JSON.encode speedscopeFile)

withIpeDB ::
  forall r.
  Maybe IpeDBOpts ->
  ( DB.Table IP.InfoProvId IP.InfoProv ->
    IO r
  ) ->
  IO r
withIpeDB = \case
  Nothing -> \action ->
    -- Create a database session.
    DB.withNewSession def $ \session ->
      -- Create the database table.
      DB.withNewTable session def $ \table ->
        action table
  Just (IpeDBOpts ipeDBPath ipeDBFormat) -> \action ->
    -- Create a database session.
    DB.withNewSession def $ \session ->
      -- Create a database table.
      DB.withTableFrom session ipeDBPath ipeDBFormat $ \table ->
        action table

-- | Process an eventlog from the given `EventlogSource` into an `EventlogProfile`.
processEventlogProfile ::
  -- | How to aggregate the stack profile samples
  AggregationMode ->
  -- | Delimiting markers. No events before a user marker containing the first
  -- string will be included. No events after a user marker containing the
  -- second string will be included.
  (Maybe Text, Maybe Text) ->
  -- | Only consider events which satisfy this predicate
  (E.EventInfo -> Bool) ->
  -- | Table with IPE data.
  DB.Table IP.InfoProvId IP.InfoProv ->
  EventlogSource ->
  IO EventlogProfile
processEventlogProfile aggregationMode (isolateStart, isolateEnd) considerEvent infoProvTable eventlogSource = do
  -- Process the eventlog.
  (profile, eventlogErrors) <-
    fmap (processEventlogProfileState aggregationMode) $
      withEventlogSource eventlogSource $ \eventlogHandle ->
        fmap last . runT $
          fromHandle eventlogHandle
            ~> decodeEvents
            ~> delimit considerEvent (markers (isolateStart, isolateEnd))
            ~> processEvents infoProvTable
            ~> final
  logEventlogErrors eventlogErrors
  pure profile

-- | Process an `EventlogProfileState` into an `EventlogProfile`.
processEventlogProfileState ::
  AggregationMode ->
  EventlogProfileState ->
  (EventlogProfile, [[EventlogError]])
processEventlogProfileState aggregationMode st = (profile, st.processingErrors)
 where
  programName :: Text
  programName = fromMaybe "" st.maybeProgramName

  profile =
    EventlogProfile
      { programName = programName
      , speedscopeFrames =
          map snd . sortOn fst $
            [ (stackFrameId, toSpeedscopeFrame stackFrame)
            | (stackFrame, stackFrameId) <- Map.toList st.stackFrames
            ]
      , speedscopeProfiles =
          toSpeedscopeProfiles programName st.samples aggregationMode
      , programVersion = Text.pack $ "ghc-stack-profiler-speedscope@" ++ CURRENT_PACKAGE_VERSION
      }

-- | Pretty-print the eventlog processing errors.
logEventlogErrors :: [[EventlogError]] -> IO ()
logEventlogErrors = \case
  [] -> pure ()
  eventlogErrorStacks -> do
    IO.hPutStrLn IO.stderr $ show (length eventlogErrorStacks) ++ " CallStacks were decoded incompletely or not at all."
    for_ eventlogErrorStacks $ \eventlogErrorStack -> do
      let
        size = length eventlogErrorStack
      for_ (take 10 eventlogErrorStack) $ \err -> do
        IO.hPutStrLn IO.stderr ("  " ++ prettyEventlogError err)
      when (size > 10) $ do
        IO.hPutStrLn IO.stderr ("  ... and " ++ show (size - 10) ++ " more.")

-- | Proces an `Event` stream and update the `EventlogProfileState`.
processEvents ::
  DB.Table IP.InfoProvId IP.InfoProv ->
  ProcessT IO E.Event EventlogProfileState
processEvents infoProvTable =
  autoT $
    scanMealyTM (handleEvent infoProvTable) emptyEventlogProfileState

-- | Handle an `Event` and update the `EventlogProfileState`.
handleEvent ::
  DB.Table IP.InfoProvId IP.InfoProv ->
  EventlogProfileState ->
  E.Event ->
  IO EventlogProfileState
handleEvent infoProvTable st ev =
  case ev.evSpec of
    E.ProgramArgs{args = programName : _} ->
      pure st{maybeProgramName = Just programName}
    E.RtsIdentifier{rtsident} ->
      pure st{maybeRtsVersion = parseIdent rtsident}
    E.UserBinaryMessage bs ->
      case GSP.deserializeEventlogMessage $ BSL.fromStrict bs of
        Left _err ->
          pure st
        Right evMsg -> case evMsg of
          GSP.CallStackFinal msg -> do
            let
              (callStackMessage, elProf1) = hydrateBinaryEventlog st msg
            processCallStackMessage infoProvTable elProf1 callStackMessage
          GSP.CallStackChunk msg ->
            pure st{current_callstack_chunks = msg : current_callstack_chunks st}
          GSP.StringDef msg ->
            pure st{hydration_table = GSP.insertTextMessage msg (hydration_table st)}
          GSP.SourceLocationDef msg ->
            case GSP.insertSourceLocationMessage msg (hydration_table st) of
              Left err ->
                pure $ addDecodingErrorsForStack [fromMissingKeyError err] st
              Right newTable ->
                pure st{hydration_table = newTable}
    _ ->
      pure st

processCallStackMessage ::
  DB.Table IP.InfoProvId IP.InfoProv ->
  EventlogProfileState ->
  GSP.CallStackMessage ->
  IO EventlogProfileState
processCallStackMessage infoProvTable st0 GSP.MkCallStackMessage{callThreadId, callCapabilityId, callStack} = do
  stackFramesOrErrors <- traverse (toStackFrame infoProvTable) callStack
  let
    (processingErrors, stackFrames) = partitionEithers stackFramesOrErrors
    (st1, stackFrameIds) = mapAccumR processStackFrame st0 stackFrames
    sample =
      Sample
        { sampleThreadId = callThreadId
        , sampleCapabilityId = callCapabilityId
        , -- TODO: Don't use an arbitrary cutoff, but reduce cycles within the stack.
          -- TODO: Perform cycle reduction or cutoff before the database lookups.
          sampleStack = take 1000 $ reverse stackFrameIds
        }
    st2 = addDecodingErrorsForStack processingErrors st1
  pure $ st2{samples = sample : st2.samples}

hydrateBinaryEventlog :: EventlogProfileState -> GSP.BinaryCallStackMessage -> (GSP.CallStackMessage, EventlogProfileState)
hydrateBinaryEventlog st msg =
  let
    chunks = current_callstack_chunks st
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
    -- 1. Chunk it: @[6,5] [4,3] [2,1]@
    -- 2. Write it to the eventlog in this order, so the messages are:
    --    [6,5]
    --    [4,3]
    --    [2,1]
    -- 3. When reading the eventlog, we store prepend later messages, resulting in:
    --    [2,1] [4,3] [6,5]
    -- 4. 'catCallStackMessage' reverses the individual callstack chunks to be the inverse of 'chunkCallStackMessage'
    orderedChunks = NonEmpty.reverse $ msg :| chunks
    fullBinaryCallStackMessage = GSP.catCallStackMessage orderedChunks
    (callStackMessage, errs) =
      GSP.hydrateEventlogCallStackMessage
        (GSP.mkIntMapSymbolTableReader (hydration_table st))
        fullBinaryCallStackMessage
  in
    ( callStackMessage
    , addDecodingErrorsForStack
        (map fromBinaryError errs)
        st{current_callstack_chunks = []}
    )

-- | Convert a `StackItem` into a `StackFrame`.
toStackFrame ::
  DB.Table IP.InfoProvId IP.InfoProv ->
  GSP.StackItem ->
  IO (Either EventlogError StackFrame)
toStackFrame infoProvTable = \case
  GSP.IpeId (toInfoProvId -> infoProvId) -> do
    maybeInfoProv <- DB.lookup infoProvTable infoProvId
    pure $
      case maybeInfoProv of
        Nothing -> Left $! UnknownInfoProvId infoProvId
        Just infoProv -> Right $! StackFrameInfoProv infoProv
  GSP.UserAnnotation (Text.pack -> message) (toSrcLoc -> srcLoc) ->
    pure . Right $! StackFrameMessage message srcLoc

-- | Ensure a `StackFrame` has a `StackFrameId` in the `EventlogProfileState`.
processStackFrame ::
  EventlogProfileState ->
  StackFrame ->
  (EventlogProfileState, StackFrameId)
processStackFrame st stackFrame =
  case Map.lookup stackFrame st.stackFrames of
    Nothing ->
      let
        !stackFrameId = st.stackFrameCounter
        !stackFrameCounter' = st.stackFrameCounter + 1
        !stackFrames' = Map.insert stackFrame stackFrameId st.stackFrames
        !st' = st{stackFrameCounter = stackFrameCounter', stackFrames = stackFrames'}
      in
        (st', stackFrameId)
    Just stackFrameId -> (st, stackFrameId)

parseIdent :: Text -> Maybe (Version, Text)
parseIdent s = convert $ listToMaybe $ flip P.readP_to_S (Text.unpack s) $ do
  void $ P.string "GHC-"
  [v1, v2, v3] <- replicateM 3 (intP <* P.optional (P.char '.'))
  P.skipSpaces
  return $ makeVersion [v1, v2, v3]
 where
  intP = read <$> P.munch1 isDigit
  convert x = (\(a, b) -> (a, Text.pack b)) <$> x

data EventlogProfile = EventlogProfile
  { programName :: Text
  , speedscopeFrames :: [Speedscope.Frame]
  , speedscopeProfiles :: [Speedscope.Profile]
  , programVersion :: Text
  }

-- | The type we wish to convert event logs into
data EventlogProfileState = EventlogProfileState
  { maybeProgramName :: Maybe Text
  , maybeRtsVersion :: Maybe (Version, Text)
  -- ^ Table of present 'InfoProv's in the eventlog
  , stackFrames :: !(Map StackFrame StackFrameId)
  -- ^ All stack frames mentioned by @ghc-stack-profiler@.
  , stackFrameCounter :: !StackFrameId
  -- ^ A unique counter for stack frames.
  , samples :: [Sample]
  -- ^ All samples in the reverse order of finding them in the eventlog.
  , hydration_table :: !GSP.IntMapTable
  -- ^ The symbol table storing 'Text' and 'SourceLocation' symbols
  -- for hydrating a 'BinaryCallStackMessage' into a 'CallStackMessage'.
  , current_callstack_chunks :: [GSP.BinaryCallStackMessage]
  -- ^ Chunks of 'BinaryCallStackMessage' we are currently decoding.
  -- All chunks are assumed to be from the same callstack and will be decoded once a
  -- 'CallStackFinal' message is encountered.
  , processingErrors :: [[EventlogError]]
  }

emptyEventlogProfileState :: EventlogProfileState
emptyEventlogProfileState =
  EventlogProfileState
    { maybeProgramName = Nothing
    , maybeRtsVersion = Nothing
    , stackFrames = Map.empty
    , stackFrameCounter = 0
    , samples = []
    , hydration_table = GSP.emptyIntMapTable
    , current_callstack_chunks = []
    , processingErrors = []
    }

addDecodingErrorsForStack :: [EventlogError] -> EventlogProfileState -> EventlogProfileState
addDecodingErrorsForStack errs elProf
  | null errs = elProf
  | otherwise = elProf{processingErrors = errs : processingErrors elProf}

--------------------------------------------------------------------------------
-- Decoding errors
--------------------------------------------------------------------------------

data EventlogError
  = UnknownInfoProvId IP.InfoProvId
  | UnknownStringId GSP.StringId
  | UnknownSrcLocId GSP.SourceLocationId
  | SourceLocationPartUndefined GSP.SourceLocationId GSP.StringId
  deriving (Show, Eq, Ord)

fromBinaryError :: GSP.BinaryCallStackDecodeError -> EventlogError
fromBinaryError = \case
  GSP.StringIdNotFound sid -> UnknownStringId sid
  GSP.SourceLocationIdNotFound sid -> UnknownSrcLocId sid

fromMissingKeyError :: GSP.MissingKeyError -> EventlogError
fromMissingKeyError = \case
  GSP.KeyStringIdNotFound sourceLocId stringId -> SourceLocationPartUndefined sourceLocId stringId

prettyEventlogError :: EventlogError -> String
prettyEventlogError = \case
  UnknownInfoProvId ipeId -> show $ UnknownInfoProvId ipeId
  UnknownStringId sid -> show $ UnknownStringId sid
  UnknownSrcLocId sid -> show $ UnknownSrcLocId sid
  SourceLocationPartUndefined srcLocId stringId ->
    "While decoding source location " ++ show srcLocId ++ ", failed to find string " ++ show stringId

--------------------------------------------------------------------------------
-- Conversion to Speedscope Types
--------------------------------------------------------------------------------

toSpeedscopeFile :: EventlogProfile -> Speedscope.File
toSpeedscopeFile profile =
  Speedscope.File
    { shared = Speedscope.Shared{frames = speedscopeFrames profile}
    , profiles = speedscopeProfiles profile
    , name = Just profile.programName
    , activeProfileIndex = Just 0
    , exporter = Just $ programVersion profile
    }

type SpeedscopeSample = (SpeedscopeSampleId, [SpeedscopeSampleStack])
type SpeedscopeSampleId = Word64
type SpeedscopeSampleStack = [Int]

toSpeedscopeProfiles :: Text -> [Sample] -> AggregationMode -> [Speedscope.Profile]
toSpeedscopeProfiles programName samples aggregationMode =
  toSpeedscopeProfile <$> speedscopeSamples
 where
  toSpeedscopeProfile :: SpeedscopeSample -> Speedscope.Profile
  toSpeedscopeProfile (sampleId, sampleStack) =
    Speedscope.SampledProfile
      Speedscope.MkSampledProfile
        { unit = Speedscope.Nanoseconds
        , name = programName <> " " <> Text.show sampleId
        , startValue = 0
        , endValue = length samples
        , weights = replicate (length samples) 1
        , samples = sampleStack
        }

  speedscopeSamples :: [SpeedscopeSample]
  speedscopeSamples =
    case aggregationMode of
      -- NOTE: groupSort is assumed to be stable
      PerThread -> groupSort $ toThreadSample <$> reverse samples
      PerCapability -> groupSort $ toCapabilitySample <$> reverse samples
      NoAggregation -> [(1, toSingleProfileSample <$> reverse samples)]
   where
    toThreadSample :: Sample -> (Word64, [Int])
    toThreadSample sample = (sample.sampleThreadId, toSingleProfileSample sample)

    toCapabilitySample :: Sample -> (Word64, [Int])
    toCapabilitySample sample = (coerce sample.sampleCapabilityId, toSingleProfileSample sample)

    toSingleProfileSample :: Sample -> [Int]
    toSingleProfileSample sample = map (GSP.word64ToInt . coerce) sample.sampleStack

toSpeedscopeFrame :: StackFrame -> Speedscope.Frame
toSpeedscopeFrame = \case
  StackFrameMessage message srcLoc ->
    Speedscope.Frame
      { name = message
      , file = srcLocFile srcLoc
      , col = srcLocColumn srcLoc
      , line = srcLocLine srcLoc
      }
  StackFrameInfoProv ip ->
    Speedscope.Frame
      { name = ip.ipModule <> " " <> if Text.null ip.ipLabel then ip.ipName else ip.ipLabel
      , file = srcLocFile ip.ipSrcLoc <|> Just ip.ipModule
      , col = srcLocColumn ip.ipSrcLoc
      , line = srcLocLine ip.ipSrcLoc
      }

--------------------------------------------------------------------------------
-- Eventlog Processing
--------------------------------------------------------------------------------

-- | Stream a file as chunks.
fromHandle :: IO.Handle -> M.SourceT IO BS.ByteString
fromHandle h =
  M.MachineT $ do
    chunks <- liftIO (BSL.toChunks <$> BSL.hGetContents h)
    M.runMachineT $ M.source chunks

-- | Parse t`E.Event`s from a stream of `BS.ByteString` chunks.
decodeEvents :: M.Process BS.ByteString E.Event
decodeEvents = M.construct $ loop E.decodeEventLog
 where
  loop :: E.Decoder a -> M.PlanT (M.Is BS.ByteString) a m ()
  loop E.Done{} = pure ()
  loop (E.Consume k) = M.await >>= \chunk -> loop (k chunk)
  loop (E.Produce a d') = M.yield a >> loop d'
  loop (E.Error _ err) = error err

-- | Delimit an `E.Event` stream using user markers and a `Moore` machine.
delimit ::
  (Monad m) =>
  -- | Only emit events which pass this predicate
  (E.EventInfo -> Bool) ->
  -- | Only emit events when this 'Moore' process state is 'True'. The process
  -- will be given the values of 'UserMarker's in the event log.
  Moore Text Bool ->
  ProcessT m E.Event E.Event
delimit p =
  construct . go
 where
  go :: (Monad m) => Moore Text Bool -> PlanT (Is E.Event) E.Event m ()
  go mm@(Moore open step) =
    await >>= \case
      ev
        -- on marker step the moore machine.
        | E.UserMarker marker <- ev.evSpec
        , mm'@(Moore open' _) <- step marker -> do
            -- if current or next state is open, emit the marker.
            when (open || open') $ yield ev
            go mm'

        -- for other events, emit if the state is open and predicate passes.
        | otherwise -> do
            when (open || p ev.evSpec) $ yield ev
            go mm

-- | A Moore machine whose state indicates which delimiting markers have been
-- seen. If both markers are 'Nothing', then the state will always be 'True'.
-- If the first marker is given, then the state will always be 'False' until a
-- value which the marker is a prefix of is seen. If the second marker is given,
-- then the state will always be 'False' after a value which the marker is a
-- prefix of is seen.
markers :: (Maybe Text, Maybe Text) -> Moore Text Bool
markers (Nothing, Nothing) =
  open
 where
  open = Moore True (const open)
markers (Just s, Nothing) =
  closedUntilOpen
 where
  closedUntilOpen =
    Moore False $ \s' ->
      if s `Text.isPrefixOf` s'
        then
          open
        else
          closedUntilOpen
  open = Moore True (const open)
markers (Nothing, Just e) =
  openUntilClosed
 where
  openUntilClosed =
    Moore True $ \e' ->
      if e `Text.isPrefixOf` e'
        then
          closed
        else
          openUntilClosed
  closed = Moore False (const closed)
markers (Just s, Just e) =
  closedUntilOpen
 where
  closedUntilOpen =
    Moore False $ \s' ->
      if s `Text.isPrefixOf` s'
        then openUntilClosed
        else closedUntilOpen
  openUntilClosed =
    Moore True $ \e' ->
      if e `Text.isPrefixOf` e'
        then closed
        else openUntilClosed
  closed = Moore False (const closed)
