{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE CPP #-}
module GHC.Internal.Stack.Decode.Compat (
  StackFrameLocation,
  StackSnapshot#,
  StackInfoTable(..),
  getInfoTableForStack,
  getInfoTableOnStack,
  advanceStackFrameLocation,
  stackHead,
  Box(..),
  getClosureBox,
) where

import GHC.Exts
import GHC.Stack.CloneStack (StackSnapshot (..))
-- See Note [No way-dependent imports]
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable
#endif
import qualified GHC.Internal.InfoProv.Types as InfoProv
import GHC.Internal.Heap.Closures.Compat
import GHC.Internal.Stack.Constants.Compat (WordOffset)

{-
Note [No way-dependent imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`ghc -M` currently assumes that the imports for a module are the same
in every way.  This is arguably a bug, but breaking this assumption by
importing different things in different ways can cause trouble.  For
example, this module in the profiling way imports and uses
GHC.Exts.Heap.InfoTableProf.  When it was not also imported in the
vanilla way, there were intermittent build failures due to this module
being compiled in the profiling way before GHC.Exts.Heap.InfoTableProf
in the profiling way. (#15197)
-}

type StackFrameLocation = (StackSnapshot, WordOffset)

data StackInfoTable = StackInfoTable
  { infoTableStructPtr :: Ptr InfoProv.StgInfoTable
  , infoTablePtr :: Ptr InfoProv.StgInfoTable
  , infoTable :: StgInfoTable
  }

-- | Get the 'StgInfoTable' of the stack frame.
-- Additionally, provides 'InfoProv' for the 'StgInfoTable' if there is any.
getInfoTableOnStack :: StackSnapshot# -> WordOffset -> IO StackInfoTable
getInfoTableOnStack stackSnapshot# index = do
  let !(# itbl_struct#, itbl_ptr_ipe_key# #) = getInfoTableAddrs# stackSnapshot# (wordOffsetToWord# index)
      itbl_struct = Ptr itbl_struct#
      itbl_ptr = Ptr itbl_ptr_ipe_key#

  itbl <- peekItbl itbl_struct
  pure StackInfoTable
    { infoTableStructPtr = itbl_struct
    , infoTablePtr = itbl_ptr
    , infoTable = itbl
    }

getInfoTableForStack :: StackSnapshot# -> IO StgInfoTable
getInfoTableForStack stackSnapshot# =
  peekItbl $
    Ptr (getStackInfoTableAddr# stackSnapshot#)

-- | Advance to the next stack frame (if any)
advanceStackFrameLocation :: StackFrameLocation -> Maybe StackFrameLocation
advanceStackFrameLocation (StackSnapshot stackSnapshot#, index) =
  let !(# s', i', hasNext #) = advanceStackFrameLocation# stackSnapshot# (wordOffsetToWord# index)
   in if I# hasNext > 0
        then Just (StackSnapshot s', primWordToWordOffset i')
        else Nothing
  where
    primWordToWordOffset :: Word# -> WordOffset
    primWordToWordOffset w# = fromIntegral (W# w#)

-- | `StackFrameLocation` of the top-most stack frame
stackHead :: StackSnapshot# -> StackFrameLocation
stackHead s# = (StackSnapshot s#, 0) -- GHC stacks are never empty

getClosureBox :: StackSnapshot# -> WordOffset -> Box
getClosureBox stackSnapshot# index =
  case getStackClosure# stackSnapshot# (wordOffsetToWord# index) of
    -- c needs to be strictly evaluated, otherwise a thunk gets boxed (and
    -- will later be decoded as such)
    !c -> Box c

-- | Advance to the next stack frame (if any)
--
-- The last `Int#` in the result tuple is meant to be treated as bool
-- (has_next).
foreign import prim "advanceStackFrameLocationzh"
  advanceStackFrameLocation# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

foreign import prim "getInfoTableAddrszh"
  getInfoTableAddrs# :: StackSnapshot# -> Word# -> (# Addr#, Addr# #)

foreign import prim "getStackInfoTableAddrzh"
  getStackInfoTableAddr# :: StackSnapshot# -> Addr#

foreign import prim "getStackClosurezh"
  getStackClosure# :: StackSnapshot# -> Word# ->  Any

-- ----------------------------------------------------------------------------
-- Utilities that really should live somewhere else
-- ----------------------------------------------------------------------------

-- | Unbox 'Int#' from 'Int'
toInt# :: Int -> Int#
toInt# (I# i) = i

-- | Convert `Int` to `Word#`
intToWord# :: Int -> Word#
intToWord# i = int2Word# (toInt# i)

wordOffsetToWord# :: WordOffset -> Word#
wordOffsetToWord# wo = intToWord# (fromIntegral wo)
