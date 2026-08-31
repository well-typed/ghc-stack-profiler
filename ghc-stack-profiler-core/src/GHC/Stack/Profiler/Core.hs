module GHC.Stack.Profiler.Core (
  -- * Eventlog Protocol Messages
  CallStack (..),
  ThreadId (..),
  CapabilityId (..),
  StackItem (..),
  IpeId (..),
  SourceLocation (..),

  -- * Binary Eventlog Protocol Messages
  Message (..),
  CallStackChunk (..),
  StringDef (..),
  BinarySourceLocationMessage (..),
  BinaryStackItem (..),
  StringId (..),
  SourceLocationId (..),

  -- * Decode

  -- ** Deserialise
  deserializeEventlogMessage,
  joinCallStackChunks,

  -- ** Hydrate
  hydrateEventlogCallStackMessage,
  BinaryCallStackDecodeError (..),

  -- *** Symbol Table
  SymbolTableReader (..),
  IntMapTable,
  mkIntMapSymbolTableReader,
  emptyIntMapTable,
  insertTextMessage,
  insertSourceLocationMessage,
  MissingKeyError (..),

  -- * Encode

  -- ** Serialise
  callStackSizeLimit,
  callStackSizeLimit_,
  eventlogBufferSize,
  chunkCallStackMessage_,

  -- ** Dehydrate
  dehydrateCallStackMessage,

  -- *** Symbol Table
  SymbolTableWriter (..),
  emptyMapSymbolTableWriter,
  MapTable,
  getKnownStrings,
  getKnownSourceLocations,
) where

import GHC.Stack.Profiler.Core.CallStack (
  CallStack (..),
  SourceLocation (..),
  StackItem (..),
 )
import GHC.Stack.Profiler.Core.Dehydrate (
  MapTable,
  SymbolTableWriter (..),
  chunkCallStackMessage_,
  dehydrateCallStackMessage,
  emptyMapSymbolTableWriter,
  getKnownSourceLocations,
  getKnownStrings,
 )
import GHC.Stack.Profiler.Core.Eventlog (
  BinarySourceLocationMessage (..),
  BinaryStackItem (..),
  CallStackChunk (..),
  CapabilityId (..),
  IpeId (..),
  Message (..),
  SourceLocationId (..),
  StringDef (..),
  StringId (..),
  ThreadId (..),
  callStackSizeLimit,
  callStackSizeLimit_,
  joinCallStackChunks,
  deserializeEventlogMessage,
  eventlogBufferSize,
 )
import GHC.Stack.Profiler.Core.Hydrate (
  BinaryCallStackDecodeError (..),
  IntMapTable,
  MissingKeyError (..),
  SymbolTableReader (..),
  emptyIntMapTable,
  hydrateEventlogCallStackMessage,
  insertSourceLocationMessage,
  insertTextMessage,
  mkIntMapSymbolTableReader,
 )
