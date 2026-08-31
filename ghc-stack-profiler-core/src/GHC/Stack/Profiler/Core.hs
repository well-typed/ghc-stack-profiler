module GHC.Stack.Profiler.Core (
  -- * Eventlog Protocol Messages
  CallStackMessage (..),
  ThreadId (..),
  CapabilityId (..),
  StackItem (..),
  IpeId (..),
  SourceLocation (..),

  -- * Binary Eventlog Protocol Messages
  BinaryEventlogMessage (..),
  BinaryCallStackMessage (..),
  BinaryStringMessage (..),
  BinarySourceLocationMessage (..),
  BinaryStackItem (..),
  StringId (..),
  SourceLocationId (..),

  -- * Decode

  -- ** Deserialise
  deserializeEventlogMessage,
  catCallStackMessage,

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
  CallStackMessage (..),
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
  BinaryCallStackMessage (..),
  BinaryEventlogMessage (..),
  BinarySourceLocationMessage (..),
  BinaryStackItem (..),
  BinaryStringMessage (..),
  CapabilityId (..),
  IpeId (..),
  SourceLocationId (..),
  StringId (..),
  ThreadId (..),
  callStackSizeLimit,
  callStackSizeLimit_,
  deserializeEventlogMessage,
  eventlogBufferSize,
 )
import GHC.Stack.Profiler.Core.Hydrate (
  BinaryCallStackDecodeError (..),
  IntMapTable,
  MissingKeyError (..),
  SymbolTableReader (..),
  catCallStackMessage,
  emptyIntMapTable,
  hydrateEventlogCallStackMessage,
  insertSourceLocationMessage,
  insertTextMessage,
  mkIntMapSymbolTableReader,
 )
