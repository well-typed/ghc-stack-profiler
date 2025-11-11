{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}
#include "Rts.h"

module GHC.Internal.InfoProv.Types.Compat (
    lookupIpeId
) where

import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.Types
#if MIN_VERSION_ghc_internal(9,1500,0)
import Foreign.Storable (peekByteOff)
#else
import GHC.Stack.Profiler.Util (castPtrToWord64)
#endif

import qualified GHC.Internal.InfoProv.Types as InfoProv

foreign import ccall "lookupIPE" c_lookupIPE ::
  Ptr InfoProv.StgInfoTable ->
  Ptr InfoProv.InfoProvEnt ->
  IO CBool

lookupIpeId :: Ptr InfoProv.StgInfoTable -> IO (Maybe Word64)
lookupIpeId itbl =
  allocaBytes (#size InfoProvEnt) $ \p -> do
    res <- c_lookupIPE itbl p
    case res of
      1 ->
#if MIN_VERSION_ghc_internal(9,1500,0)
        Just `fmap` peekIpProvId (InfoProv.ipeProv p)
#else
        pure $ Just $ castPtrToWord64 itbl
#endif
      _ -> return Nothing

#if MIN_VERSION_ghc_internal(9,1500,0)
peekIpProvId :: Ptr InfoProv.InfoProv -> IO Word64
peekIpProvId p  =  (# peek InfoProv, info_prov_id) p
#endif
