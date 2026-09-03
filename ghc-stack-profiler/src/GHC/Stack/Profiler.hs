module GHC.Stack.Profiler (
  -- * High-Level API

  -- ** Profiler
  Profiler (..),
  withProfiler,
  withProfilerWith,
  startProfiler,
  startProfilerWith,
  stopProfiler,

  -- ** Options
  Options (
    shouldStart,
    shouldSample,
    sampleRtsThreads,
    sampleProfilerThreads,
    sampleInterval
  ),
  defaultOptions,
  ThreadLabel,
  ShouldSample (..),
  Interval (..),

  -- * Low-Level API

  -- ** Manager
  Manager,
  withManager,
  startManager,
  stopManager,

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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import GHC.Conc
import GHC.Conc.Sync (threadLabel)
import GHC.IsList (IsList (..))
import qualified GHC.Stack.Profiler.Eventlog.Socket as Eventlog.Socket
import GHC.Stack.Profiler.Manager
import GHC.Stack.Profiler.Sampler (Interval (MkIntervalMillis), SamplerDescr (..), startSampler, stopSampler, withSampler)
import GHC.Stack.Profiler.Util (DList, WriterT, runWriterT, tell)

-------------------------------------------------------------------------------
-- High-level API
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Profiler

-- | A profiler handle, which can be used to stop the profiler with `stopProfiler`.
data Profiler = MkProfiler
  { profilerManager :: !Manager
  , profilerSampler :: !Sampler
  }

-- | Run an action with a `Profiler` and the default `Options`.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
withProfiler :: IO a -> IO a
withProfiler action =
  bracket startProfiler stopProfiler (const action)

-- | Variant of `withProfiler` that accepts `Options`.
withProfilerWith :: Options -> IO a -> IO a
withProfilerWith options action =
  bracket (startProfilerWith options) stopProfiler (const action)

-- | Start a `Profiler` with the default `Options`.
--
--   This function returns a `Profiler` handle, which can be used to stop
--   the profiler with `stopProfiler`.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
startProfiler :: IO Profiler
startProfiler =
  startProfilerWith defaultOptions

-- | Variant of `startProfiler` that accepts `Options`.
startProfilerWith :: Options -> IO Profiler
startProfilerWith options = do
  profilerManager <- startManager (shouldStart options)
  profilerSampler <- startSamplerWith profilerManager options
  pure MkProfiler{profilerManager, profilerSampler}

-- | Stop a `Profiler`.
stopProfiler :: Profiler -> IO ()
stopProfiler MkProfiler{profilerManager, profilerSampler} = do
  stopSampler profilerManager profilerSampler
  stopManager profilerManager

-------------------------------------------------------------------------------
-- Options

-- | The options `withProfilerWith` and `startProfilerWith`.
--
--   To construct options, modify `defaultOptions` using the fields:
--
--   [@`GHC.Stack.Profiler.shouldStart` :: `Bool`@]:
--     Determines if sampler threads are started on creation or wait for a
--     "start profiling" command on the eventlog socket. If you are using
--     @ghc-stack-profiler@ with @eventlog-socket@'s control commands, this
--     should be set to @False@. Otherwise, this should be @True@. The default
--     is @True@.
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
data Options = MkOptions
  { shouldStart :: !Bool
  , shouldSample :: ThreadId -> Maybe ThreadLabel -> ShouldSample
  , sampleRtsThreads :: !Bool
  , sampleProfilerThreads :: !Bool
  , sampleInterval :: !Interval
  }

-- | The default `Options`. See `Options` for the default values.
defaultOptions :: Options
defaultOptions =
  MkOptions
    { shouldStart = True
    , shouldSample = \_threadId _maybeThreadLabel -> Yes
    , sampleRtsThreads = False
    , sampleProfilerThreads = False
    , sampleInterval = MkIntervalMillis 10
    }

-- | A thread label, as set by `labelThread`.
type ThreadLabel = String

-- | A flag to indicate whether or not a thread should be sampled.
--
--   Used in the `shouldSample` field of `Options`.
data ShouldSample
  = -- | The thread should be sampled.
    Yes
  | -- | The thread should not be sampled.
    No
  | -- | The thread should never be sampled.
    Never

-------------------------------------------------------------------------------
-- Low-level API
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Manager

-- | Run an action with a new `Manager`.
--
--   The first argument determines if sampler threads are started on creation
--   or wait for a "start profiling" command on the eventlog socket.
--   If you are using @ghc-stack-profiler@ with @eventlog-socket@'s control
--   commands, this should be set to @False@. Otherwise, this should be @True@.
--
--   The `Manager` is stopped when the action finishes.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
withManager ::
  -- | Flag that determines if sampler threads are started on creation.
  Bool ->
  -- | The action that runs with the `Manager`.
  (Manager -> IO a) ->
  IO a
withManager shouldStart action =
  bracket (startManager shouldStart) stopManager action

-- | Start a `Manager`.
--
--   The first argument determines if sampler threads are started on creation
--   or wait for a "start profiling" command on the eventlog socket.
--   If you are using @ghc-stack-profiler@ with @eventlog-socket@'s control
--   commands, this should be set to @False@. Otherwise, this should be @True@.
--
--   __Warning:__ The manager must be stopped with `stopManager`.
--
--   __Warning:__ This function spawns a `Manager` thread.
--   Having multiple concurrent `Manager` threads is unsupported and unsafe.
startManager :: Bool -> IO Manager
startManager shouldStart = do
  -- TODO: Detect if the event loop thread is running and throw an error.
  manager <- newManager shouldStart
  startEventLoop manager
  Eventlog.Socket.registerWithEventlogSocket manager
  pure manager

-------------------------------------------------------------------------------
-- Sampler
-------------------------------------------------------------------------------

-- | Run an action with a `Sampler` for the _current thread_.
--
--   The `Sampler` is stopped when the action finishes.
--
--   __Warning:__ If the action creates a new thread, it _will not_ be sampled.
withSamplerForMe :: Manager -> Interval -> IO a -> IO a
withSamplerForMe manager interval action = do
  myThreadId >>= \threadId ->
    withSampler (samplerFor manager threadId interval) (const action)

-- | Start a sampler for the given `ThreadId`.
--
--   __Warning:__ The sampler must be stopped using `stopSampler`.
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
--   This function ignores the `shouldStart` field and uses the value that was
--   passed to the `Manager` on creation.
--
--   __Warning:__ The sampler must be stopped using `stopSampler`.
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
  MkOptions{shouldSample, sampleRtsThreads, sampleProfilerThreads, sampleInterval} = options

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
              if sampleProfilerThreads
                then yes threadId -- Sample it.
                else never threadId -- Add it to the neverSet.
            else do
              maybeThreadLabel <- liftIO (threadLabel threadId)
              -- If the threadId is an RTS thread,
              -- it should be sampled if-and-only-if shouldSampleRtsThreads is true.
              if isRtsThread maybeThreadLabel
                then
                  if sampleRtsThreads
                    then yes threadId -- Sample it.
                    else never threadId -- Add it to the neverSet.
                else
                  -- Otherwise, run the user-provided predicate and follow its instructions.
                  case shouldSample threadId maybeThreadLabel of
                    Yes -> yes threadId
                    No -> no threadId
                    Never -> never threadId

    yes, no, never :: ThreadId -> WriterT (DList ThreadId) IO (Maybe ThreadId)
    yes threadId = pure $ Just threadId
    no _threadId = pure Nothing
    never threadId = tell (fromList [threadId]) >> pure Nothing

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
