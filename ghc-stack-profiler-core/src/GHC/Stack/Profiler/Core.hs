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

  -- * Serialisation
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

  -- * Deserialisation
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
) where

import GHC.Stack.Profiler.Core.CallStack (
  BinaryCallStackDecodeError (..),
  CallStackMessage (..),
  StackItem (..),
  catCallStackMessage,
  chunkCallStackMessage_,
  dehydrateCallStackMessage,
  hydrateEventlogCallStackMessage,
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
import GHC.Stack.Profiler.Core.SourceLocation (
  SourceLocation (..),
 )
import GHC.Stack.Profiler.Core.SymbolTable (
  IntMapTable,
  MapTable,
  MissingKeyError (..),
  SymbolTableReader (..),
  SymbolTableWriter (..),
  emptyIntMapTable,
  emptyMapSymbolTableWriter,
  getKnownSourceLocations,
  getKnownStrings,
  insertSourceLocationMessage,
  insertTextMessage,
  mkIntMapSymbolTableReader,
 )
