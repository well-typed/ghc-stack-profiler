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
  SymbolTableReader (..),
  IntMapTable,
  BinaryCallStackDecodeError (..),
  MissingKeyError (..),
  catCallStackMessage,
  deserializeEventlogMessage,
  hydrateEventlogCallStackMessage,
  mkIntMapSymbolTableReader,
  emptyIntMapTable,
  insertSourceLocationMessage,
  insertTextMessage,

  -- * Encode
  dehydrateCallStackMessage,
  SymbolTableWriter (..),
  MapTable,
  emptyMapSymbolTableWriter,
  getKnownStrings,
  getKnownSourceLocations,
  callStackSizeLimit,
  callStackSizeLimit_,
  eventlogBufferSize,
  chunkCallStackMessage_,
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
