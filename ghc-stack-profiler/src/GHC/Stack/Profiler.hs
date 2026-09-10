module GHC.Stack.Profiler (
  -- * High-Level API

  -- ** Profiler
  Profiler (..),
  withProfiler,
  withProfilerWith,
  withProfilerFromEnv,
  startProfiler,
  startProfilerWith,
  startProfilerFromEnv,
  stopProfiler,

  -- ** Options
  Options (
    wait,
    shouldSample,
    sampleRtsThreads,
    sampleProfilerThreads,
    sampleInterval
  ),
  defaultOptions,
  Interval (..),

  -- *** Thread Filters and Glob Patterns
  ThreadFilter,
  ThreadLabel,
  ShouldSample (..),
  Glob,
  matches,
  sampleInclude,
  sampleExclude,
  sampleIncludeExclude,

  -- *** Environment Variables
  fromEnv,

  -- * Low-Level API

  -- ** Manager
  Manager,
  withManager,
  startManager,
  stopManager,

  -- ** Commands
  startProfiling,
  stopProfiling,

  -- ** Samplers
  Sampler,
  withSamplerForMe,
  startSamplerFor,
  startSamplerWith,
  stopSampler,
) where

import Control.Concurrent.Async (Async (..))
import Control.Exception
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.String (IsString (..))
import GHC.Conc
import GHC.Conc.Sync (threadLabel)
import GHC.IsList (IsList (..))
import qualified GHC.Stack.Profiler.Internal.Eventlog.Socket as Eventlog.Socket
import GHC.Stack.Profiler.Internal.Manager
import GHC.Stack.Profiler.Internal.Sampler (Interval (MkIntervalMillis), SamplerDescr (MkSamplerDescr), startSampler, stopSampler, withSampler)
import qualified GHC.Stack.Profiler.Internal.Sampler as SamplerDescr
import GHC.Stack.Profiler.Internal.Util (DList, Glob, WriterT, matches, runWriterT, tell)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- High-level API
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Profiler

-- | A profiler handle, which can be used to stop the profiler with `stopProfiler`.
--
--   @since 0.5.0.0
data Profiler = MkProfiler
  { profilerManager :: !Manager
  , profilerSampler :: !Sampler
  }

-- | Run an action with a `Profiler` and the default `Options`.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
--
--   @since 0.5.0.0
withProfiler :: (Profiler -> IO a) -> IO a
withProfiler action =
  bracket startProfiler stopProfiler action

-- | Variant of `withProfiler` that accepts `Options`.
--
--   @since 0.5.0.0
withProfilerWith :: Options -> (Profiler -> IO a) -> IO a
withProfilerWith options action =
  bracket (startProfilerWith options) stopProfiler action

-- | Variant of `withProfiler` that reads `Options` from the environment.
--
--   If @GHC_STACK_PROFILER@ is unset or empty, no `Profiler` is started.
--
--   @since 0.5.0.0
withProfilerFromEnv :: (Maybe Profiler -> IO a) -> IO a
withProfilerFromEnv action =
  bracket startProfilerFromEnv (traverse_ stopProfiler) action

-- | Start a `Profiler` with the default `Options`.
--
--   This function returns a `Profiler` handle, which can be used to stop
--   the profiler with `stopProfiler`.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
--
--   @since 0.5.0.0
startProfiler :: IO Profiler
startProfiler =
  startProfilerWith defaultOptions

-- | Variant of `startProfiler` that accepts `Options`.
--
--   @since 0.5.0.0
startProfilerWith :: Options -> IO Profiler
startProfilerWith options = do
  profilerManager <- startManager (wait options)
  profilerSampler <- startSamplerWith profilerManager options
  pure MkProfiler{profilerManager, profilerSampler}

-- | Variant of `startProfiler` that accepts `Options`.
--
--   If @GHC_STACK_PROFILER@ is unset or empty, no `Profiler` is started.
--
--   @since 0.5.0.0
startProfilerFromEnv :: IO (Maybe Profiler)
startProfilerFromEnv =
  fromEnv >>= traverse startProfilerWith

-- | Stop a `Profiler`.
--
--   @since 0.5.0.0
stopProfiler :: Profiler -> IO ()
stopProfiler MkProfiler{profilerManager, profilerSampler} = do
  stopSampler profilerManager profilerSampler
  stopManager profilerManager

-------------------------------------------------------------------------------
-- Options

-- | The options for `withProfilerWith` and `startProfilerWith`.
--
--   To construct options, modify `defaultOptions` using the fields:
--
--   [@`GHC.Stack.Profiler.wait` :: `Bool`@]:
--     Determines if sampler threads are started on creation or wait for a
--     "start profiling" command on the eventlog socket. If you are using
--     @ghc-stack-profiler@ with @eventlog-socket@'s control commands, this
--     should be set to @True@. Otherwise, this should be @False@. The default
--     is @False@.
--   [@`GHC.Stack.Profiler.shouldSample` :: `ThreadId` -> `Maybe` `ThreadLabel` -> `ShouldSample`@]:
--     Determines if the thread idenfied by the `ThreadId` should be sampled.
--     The current `ThreadLabel`, returned by `threadLabel`, is passed as the
--     second argument. If this function returns `Never`, the thread will never
--     be sampled, even if its `ThreadLabel` changes. The default predicate
--     always returns `Yes`. This function is not used for RTS threads or
--     threads spawned by @ghc-stack-profiler@.
--   [@`GHC.Stack.Profiler.sampleRtsThreads` :: `Bool`@]:
--     Determines if builtin RTS threads should be sampled. The builtin RTS
--     threads are the TimerManager and IOManager threads, and do not usually
--     have an interesting call-stack profile. The default is @False@.
--   [@`GHC.Stack.Profiler.sampleProfilerThreads` :: `Bool`@]:
--     Determines if the threads spawned by @ghc-stack-profiler@ should be
--     sampled. The default is @False@.
--   [@`GHC.Stack.Profiler.sampleInterval` :: `Interval`@]:
--     Determines the sampling interval.
--     The default is @10@ milliseconds.
--
--   @since 0.5.0.0
data Options = MkOptions
  { wait :: !Bool
  , shouldSample :: ThreadFilter
  , sampleRtsThreads :: !Bool
  , sampleProfilerThreads :: !Bool
  , sampleInterval :: !Interval
  }

-- | The default `Options`. See `Options` for the default values.
--
--   @since 0.5.0.0
defaultOptions :: Options
defaultOptions =
  MkOptions
    { wait = False
    , shouldSample = \_threadId _maybeThreadLabel -> Yes
    , sampleRtsThreads = False
    , sampleProfilerThreads = False
    , sampleInterval = MkIntervalMillis 10
    }

-- | A thread filter, used to determine which threads should be sampled.
--
--   Used in the `shouldSample` field of `Options`.
--
--   @since 0.5.0.0
type ThreadFilter = ThreadId -> Maybe ThreadLabel -> ShouldSample

-- | A thread label, as set by `labelThread`.
--
--   @since 0.5.0.0
type ThreadLabel = String

-- | The result type of a `ThreadFilter`.
--
--   @since 0.5.0.0
data ShouldSample
  = -- | The thread should be sampled.
    Yes
  | -- | The thread should not be sampled.
    No
  | -- | The thread should never be sampled.
    Never

-- | Construct a thread filter from an include `Glob` pattern.
--
--   If the thread label matches the given pattern, the thread filter returns `Yes`.
--   Otherwise, the thread filter returns `No`.
--   The thread filter never returns `Never`.
--
--   @since 0.5.0.0
sampleInclude ::
  -- | The include pattern.
  Glob ->
  ThreadFilter
sampleInclude globInclude =
  const . maybe No $
    fromBool . \label ->
      globInclude `matches` label

-- | Construct a thread filter from an exclude `Glob` pattern.
--
--   If the thread label matches the given pattern, the thread filter returns `No`.
--   Otherwise, the thread filter returns `Yes`.
--   The thread filter never returns `Never`.
--
--   @since 0.5.0.0
sampleExclude ::
  -- | The exclude pattern.
  Glob ->
  ThreadFilter
sampleExclude globExclude =
  const . maybe Yes $
    fromBool . \label ->
      not (globExclude `matches` label)

-- | Construct a thread filter from include and exclude `Glob` patterns.
--
--   If the thread label matches the given include pattern and does not match
--   the given exclude pattern, the thread filter returns `Yes`.
--   Otherwise, the thread filter returns `No`.
--   The thread filter never returns `Never`.
--
--   @since 0.5.0.0
sampleIncludeExclude ::
  -- | The include pattern.
  Glob ->
  -- | The exclude pattern.
  Glob ->
  ThreadFilter
sampleIncludeExclude globInclude globExclude =
  const . maybe Yes $
    fromBool . \label ->
      globInclude `matches` label && not (globExclude `matches` label)

-- | Internal helper.
--
--   Construct a `ShouldSample` from a `Bool`.
--
--   Maps `True` to `Yes` and `False` to `No`.
fromBool :: Bool -> ShouldSample
fromBool b = if b then Yes else No

-- | Read the `Options` from the environment.
--
--   [@GHC_STACK_PROFILER@]:
--     If set to any non-empty value, read and return the options.
--     Otherwise, return `Nothing`, which indicates the `Profiler` should not be started.
--   [@GHC_STACK_PROFILER_WAIT@]:
--     If set to any non-empty value, `wait` is set to `True`.
--   [@GHC_STACK_PROFILER_SAMPLE_INCLUDE@]:
--     If set, `shouldSample` is set to the `ThreadFilter` constructed using `sampleInclude` using the value as a `Glob` pattern.
--     If @GHC_STACK_PROFILER_SAMPLE_EXCLUDE@ is also set, `sampleIncludeExclude` is used.
--   [@GHC_STACK_PROFILER_SAMPLE_EXCLUDE@]:
--     If set, `shouldSample` is set to the `ThreadFilter` constructed using `sampleExclude` using the value as a `Glob` pattern.
--     If @GHC_STACK_PROFILER_SAMPLE_INCLUDE@ is also set, `sampleIncludeExclude` is used.
--   [@GHC_STACK_PROFILER_SAMPLE_RTS_THREADS@]:
--     If set to any non-empty value, `sampleRtsThreads` is set to `True`.
--   [@GHC_STACK_PROFILER_SAMPLE_PROFILER_THREADS@]:
--     If set to any non-empty value, `sampleProfilerThreads` is set to `True`.
--   [@GHC_STACK_PROFILER_SAMPLE_INTERVAL@]:
--     If set to any numeric value, `sampleInterval` is set to the `Interval` constructed using the value as milliseconds.
--     If set to any non-numeric value, a warning is printed to `stderr` and the default `sampleInterval` is used.
--
--   __Warning:__ This function reads environment variables, which is not thread-safe.
--                See [@getenv@](https://en.cppreference.com/c/program/getenv).
--
--   @since 0.5.0.0
fromEnv :: IO (Maybe Options)
fromEnv = do
  shouldStart <- testEnv startVar
  if not shouldStart
    then pure Nothing
    else do
      wait <- testEnv waitVar
      shouldSample <-
        (,) <$> lookupEnvGlob sampleIncludeVar <*> lookupEnvGlob sampleExcludeVar <&> \case
          (Nothing, Nothing) -> shouldSample defaultOptions
          (Just includeGlob, Nothing) -> sampleInclude includeGlob
          (Nothing, Just excludeGlob) -> sampleExclude excludeGlob
          (Just includeGlob, Just excludeGlob) -> sampleIncludeExclude includeGlob excludeGlob
      sampleRtsThreads <- testEnv sampleRtsThreadsVar
      sampleProfilerThreads <- testEnv sampleProfilerThreadsVar
      sampleInterval <-
        lookupEnv sampleIntervalVar >>= \case
          Nothing ->
            pure $ sampleInterval defaultOptions
          Just sampleIntervalMillisString ->
            case readMaybe sampleIntervalMillisString of
              Nothing -> do
                hPutStrLn stderr $
                  printf
                    "Could not parse the value of %s. Expected a number, found %s"
                    sampleIntervalVar
                    sampleIntervalMillisString
                pure $ sampleInterval defaultOptions
              Just sampleIntervalMillis ->
                pure $ MkIntervalMillis sampleIntervalMillis
      pure $
        Just
          MkOptions
            { wait
            , shouldSample
            , sampleRtsThreads
            , sampleProfilerThreads
            , sampleInterval
            }
 where
  testEnv :: String -> IO Bool
  testEnv = fmap (maybe False (not . null)) . lookupEnv

  lookupEnvGlob :: String -> IO (Maybe Glob)
  lookupEnvGlob = fmap (fmap fromString) . lookupEnv

  startVar :: String
  startVar = "GHC_STACK_PROFILER"

  waitVar :: String
  waitVar = "GHC_STACK_PROFILER_WAIT"

  sampleIncludeVar :: String
  sampleIncludeVar = "GHC_STACK_PROFILER_SAMPLE_INCLUDE"

  sampleExcludeVar :: String
  sampleExcludeVar = "GHC_STACK_PROFILER_SAMPLE_EXCLUDE"

  sampleRtsThreadsVar :: String
  sampleRtsThreadsVar = "GHC_STACK_PROFILER_SAMPLE_RTS_THREADS"

  sampleProfilerThreadsVar :: String
  sampleProfilerThreadsVar = "GHC_STACK_PROFILER_SAMPLE_PROFILER_THREADS"

  sampleIntervalVar :: String
  sampleIntervalVar = "GHC_STACK_PROFILER_SAMPLE_INTERVAL"

-------------------------------------------------------------------------------
-- Low-level API
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Manager

-- | Run an action with a new `Manager`.
--
--   The first argument indicates if sampler threads should wait for a call to
--  `startProfiling` or a "start profiling" command on the eventlog socket.
--   If you are using @ghc-stack-profiler@ with @eventlog-socket@'s control
--   commands, this should be set to @True@.
--
--   The `Manager` is stopped when the action finishes.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
--
--   @since 0.5.0.0
withManager ::
  -- | Flag that determines if sampler threads should wait.
  Bool ->
  -- | The action that runs with the `Manager`.
  (Manager -> IO a) ->
  IO a
withManager wait action =
  bracket (startManager wait) stopManager action

-- | Start a `Manager`.
--
--   The first argument indicates if sampler threads should wait for a call to
--  `startProfiling` or a "start profiling" command on the eventlog socket.
--   If you are using @ghc-stack-profiler@ with @eventlog-socket@'s control
--   commands, this should be set to @True@.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
--
--   __Warning:__ The manager should be stopped with `stopManager`.
--
--   @since 0.5.0.0
startManager :: Bool -> IO Manager
startManager wait = do
  -- TODO: Detect if the event loop thread is running and throw an error.
  manager <- newManager wait
  startEventLoop manager
  Eventlog.Socket.registerWithEventlogSocket manager
  pure manager

-------------------------------------------------------------------------------
-- Sampler
-------------------------------------------------------------------------------

-- | Run an action with a `Sampler` for the current thread.
--
--   The `Sampler` is stopped when the action finishes.
--
--   __Warning:__ If the action creates a new thread, it /will not/ be sampled.
--
--   @since 0.5.0.0
withSamplerForMe :: Manager -> Interval -> (Sampler -> IO a) -> IO a
withSamplerForMe manager interval action = do
  myThreadId >>= \threadId ->
    withSampler (samplerFor manager threadId interval) action

-- | Start a sampler for the given `ThreadId`.
--
--   __Warning:__ The sampler should be stopped using `stopSampler` or `stopManager`.
--
--   @since 0.5.0.0
startSamplerFor :: Manager -> ThreadId -> Interval -> IO Sampler
startSamplerFor manager threadId interval =
  startSampler (samplerFor manager threadId interval)

-- | Internal helper.
--
--   Create a `SamplerDescr` that samples a single thread.
samplerFor :: Manager -> ThreadId -> Interval -> SamplerDescr
samplerFor samplerManager threadId sampleInterval =
  MkSamplerDescr{samplerManager, samplerThreads, sampleInterval}
 where
  samplerThreads = pure [threadId]

-- | Start a sampler with the given `Options`.
--
--   This function ignores the `wait` field and uses the value that was
--   passed to the `Manager` on creation.
--
--   __Warning:__ The sampler should be stopped using `stopSampler` or `stopManager`.
--
--   @since 0.5.0.0
startSamplerWith :: Manager -> Options -> IO Sampler
startSamplerWith manager options = do
  neverSetRef <- newIORef Set.empty
  startSampler (samplerWith manager neverSetRef options)

-- | Internal helper.
--
--   Create a `SamplerDescr` for the given `Options`.
samplerWith ::
  Manager ->
  IORef (Set ThreadId) ->
  Options ->
  SamplerDescr
samplerWith samplerManager neverSetRef options =
  MkSamplerDescr{samplerManager, samplerThreads, sampleInterval}
 where
  MkOptions
    { shouldSample
    , sampleRtsThreads = fromBool -> shouldSampleRtsThreads
    , sampleProfilerThreads = fromBool -> shouldSampleProfilerThreads
    , sampleInterval
    } = options

  samplerThreads = do
    neverSet <- readIORef neverSetRef
    (threadIds', neverSet') <- filterThreads neverSet =<< listThreads
    writeIORef neverSetRef $! neverSet'
    pure threadIds'

  filterThreads :: Set ThreadId -> [ThreadId] -> IO ([ThreadId], Set ThreadId)
  filterThreads neverSet =
    fmap (bimap catMaybes (foldr S.insert neverSet . toList))
      . runWriterT
      . traverse testThread
   where
    testThread :: ThreadId -> WriterT (DList ThreadId) IO (Maybe ThreadId)
    testThread threadId
      -- If the threadId is in the neverSet, do not sample it.
      | threadId `S.member` neverSet =
          pure Nothing
      | otherwise = do
          -- If the threadId is a profiler thread,
          -- it should be sampled if-and-only-if shouldSampleProfilerThreads is true.
          isProfilerThread <- liftIO (isProfilerThreadFor samplerManager threadId)
          if isProfilerThread
            then
              evalShouldSample threadId shouldSampleProfilerThreads
            else do
              maybeThreadLabel <- liftIO (threadLabel threadId)
              -- If the threadId is an RTS thread,
              -- it should be sampled if-and-only-if shouldSampleRtsThreads is true.
              if isRtsThread maybeThreadLabel
                then
                  evalShouldSample threadId shouldSampleRtsThreads
                else
                  -- Otherwise, run the user-provided predicate and follow its instructions.
                  evalShouldSample threadId (shouldSample threadId maybeThreadLabel)

    -- Evaluate a `ShouldSample` judgement for the given threadId.
    evalShouldSample :: ThreadId -> ShouldSample -> WriterT (DList ThreadId) IO (Maybe ThreadId)
    evalShouldSample threadId = \case
      Yes -> pure (Just threadId)
      No -> pure Nothing
      Never -> tell (fromList [threadId]) >> pure Nothing

-- | Was the given thread created by this library?
isProfilerThreadFor :: Manager -> ThreadId -> IO Bool
isProfilerThreadFor manager threadId =
  atomically $ do
    isEventLoopThread <-
      fromMaybe False . fmap ((== threadId) . asyncThreadId . eventLoopAsync)
        <$> readTVar (eventLoopThreadVar manager)
    isSamplerThread <-
      Map.member threadId
        <$> readTVar (samplerThreadMapVar manager)
    pure $ isEventLoopThread || isSamplerThread

-- | Is the given thread an RTS thread?
isRtsThread :: Maybe ThreadLabel -> Bool
isRtsThread =
  maybe False (\label -> label == "TimerManager" || "IOManager on cap" `isPrefixOf` label)
