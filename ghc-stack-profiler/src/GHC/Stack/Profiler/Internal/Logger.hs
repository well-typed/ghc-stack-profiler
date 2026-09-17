module GHC.Stack.Profiler.Internal.Logger (
  Logger (..),
  logWith,
  logWithSev,
  cmapWithSev,
  Severity (..),
  WithSev (..),
  prettyWithSev,
  filterSeverity,
) where

import Data.Functor.Contravariant (Contravariant (..))

--------------------------------------------------------------------------------
-- Logger
--------------------------------------------------------------------------------

-- NOTE: Part of the public API.

-- | A logger.
--
--   @since 0.5.1.0
newtype Logger msg
  = MkLogger
  { unLogger :: msg -> IO ()
  }

logWith :: Logger msg -> msg -> IO ()
logWith logger msg = unLogger logger msg

logWithSev :: Logger (WithSev msg) -> Severity -> msg -> IO ()
logWithSev logger sev msg = logWith logger (WithSev sev msg)

cmapWithSev :: (msg' -> msg) -> Logger (WithSev msg) -> Logger (WithSev msg')
cmapWithSev f logger = contramap (fmap f) logger

instance Contravariant Logger where
  contramap :: (a' -> a) -> Logger a -> Logger a'
  contramap f (MkLogger unLog) = MkLogger (unLog . f)

instance Monoid (Logger a) where
  mempty :: Logger a
  mempty = MkLogger (\_ -> pure ())

instance Semigroup (Logger a) where
  (<>) :: Logger a -> Logger a -> Logger a
  MkLogger f <> MkLogger g = MkLogger (\msg -> f msg *> g msg)

--------------------------------------------------------------------------------
-- Severity and helpers

-- NOTE: Part of the public API.

-- | The log severity.
--
--   @since 0.5.1.0
data Severity
  = TRACE
  | DEBUG
  | INFO
  | WARN
  | ERROR
  | FATAL
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- NOTE: Part of the public API.

-- | A log message with its severity.
--
--   @since 0.5.1.0
data WithSev msg = WithSev Severity msg
  deriving (Functor)

prettyWithSev :: (msg -> String) -> WithSev msg -> String
prettyWithSev prettyMsg (WithSev sev msg)
  | sev == INFO = prettyMsg msg
  | otherwise = show sev ++ ": " ++ prettyMsg msg

--------------------------------------------------------------------------------
-- Simple output streams

filterSeverity :: (Severity -> Bool) -> Logger (WithSev msg) -> Logger (WithSev msg)
filterSeverity p (MkLogger l) =
  MkLogger $ \msg@(WithSev sev _) ->
    if p sev then l msg else pure ()
