{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}
#include "Rts.h"

module GHC.Internal.InfoProv.Types.Compat (
    lookupIpeId
) where

import Data.Word
import Foreign.Ptr
#if !MIN_VERSION_ghc_internal(9,1500,0)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import GHC.Stack.Profiler.Util (castPtrToWord64)
#endif

import qualified GHC.Internal.InfoProv.Types as InfoProv

#if MIN_VERSION_ghc_internal(9,1500,0)
foreign import ccall "lookupIPEId" c_lookupIPEId ::
  Ptr InfoProv.StgInfoTable ->
  IO Word64
#else
foreign import ccall "lookupIPE" c_lookupIPE ::
  Ptr InfoProv.StgInfoTable ->
  Ptr InfoProv.InfoProvEnt ->
  IO CBool
#endif

lookupIpeId :: Ptr InfoProv.StgInfoTable -> IO (Maybe Word64)
lookupIpeId itbl = do
#if MIN_VERSION_ghc_internal(9,1500,0)
  c_lookupIPEId itbl >>= \ case
    0 -> pure Nothing
    ipeId -> pure $ Just ipeId
#else
  allocaBytes (#size InfoProvEnt) $ \p -> do
    res <- c_lookupIPE itbl p
    case res of
      1 -> pure $ Just $ castPtrToWord64 itbl
      _ -> return Nothing
#endif
