{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Debug.Trace.Binary.Compat (
  -- * For tracing user binary events
  traceBinaryEventIO,
  -- * Flag to check whether the eventlog is enabled
  userTracingEnabled,
  ) where

#if defined(USE_GHC_TRACE_EVENTS)
import Debug.Trace.Binary (traceBinaryEventIO)
import Debug.Trace.Flags (userTracingEnabled)
#else
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import GHC.Types (IO(..))
import GHC.Exts (Ptr(..), Int(..), traceBinaryEvent#)
import GHC.Internal.RTS.Flags.Test (getUserEventTracingEnabled)
import System.IO.Unsafe (unsafePerformIO)

traceBinaryEventIO :: B.ByteString -> IO ()
traceBinaryEventIO bytes = traceBinaryEventIO' bytes

traceBinaryEventIO' :: B.ByteString -> IO ()
traceBinaryEventIO' bytes =
  BU.unsafeUseAsCStringLen bytes $ \(Ptr p, I# n) -> IO $ \s ->
    case traceBinaryEvent# p n s of
      s' -> (# s', () #)

userTracingEnabled :: Bool
userTracingEnabled = unsafePerformIO getUserEventTracingEnabled
#endif
