{-# LANGUAGE CPP #-}
module GHC.Stack.Profiler.Stack.Compat (
  lookupIpeIdForStackFrame,
) where

import Data.Binary
import GHC.Internal.InfoProv.Types.Compat
import GHC.Internal.Stack.Decode.Compat

#if !MIN_VERSION_ghc_internal(9,1500,0)
import GHC.Stack.Profiler.Util (castPtrToWord64)
#endif

lookupIpeIdForStackFrame :: StackInfoTable -> IO (Maybe Word64)
lookupIpeIdForStackFrame itbl = do
  mId <- lookupIpeId (infoTablePtr itbl)
#if MIN_VERSION_ghc_internal(9,1500,0)
  pure mId
#else
  -- In GHC <9.14, the key for looking up the InfoProv is the Ptr to the 'StgInfoTable'
  -- However, to the eventlog, we write the address of the struct.
  -- So, to check whether there is an InfoProv, we first lookup by the 'StgInfoTable' ptr,
  -- i.e. not adjusting for 'TABLES_NEXT_TO_CODE', but if there is an entry, we use the
  -- the struct address, otherwise the decoder will not be able to find the 'InfoProv'.
  pure $ castPtrToWord64 (infoTableStructPtr itbl) <$ mId
#endif
