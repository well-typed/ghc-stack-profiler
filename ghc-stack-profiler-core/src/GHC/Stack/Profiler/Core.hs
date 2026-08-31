module GHC.Stack.Profiler.Core (
  -- * Eventlog Protocol Messages
  CallStackMessage (..),
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
  callStackSizeLimit,
  callStackSizeLimit_,
  eventlogBufferSize,
  chunkCallStackMessage_,

  -- * Deserialisation
  catCallStackMessage,
  deserializeEventlogMessage,
  hydrateEventlogCallStackMessage,
  BinaryCallStackDecodeError (..),

  -- * Symbol Tables
  SymbolTableWriter (..),
  SymbolTableReader (..),

  -- ** Symbol Tables using 'Data.Map.Strict.Map'
  MapTable,
  emptyMapSymbolTableWriter,
  getKnownStrings,
  getKnownSourceLocations,

  -- ** Symbol Tables using 'Data.IntMap.Strict.IntMap'
  IntMapTable,
  MissingKeyError (..),
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
  deserializeEventlogMessage,
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
  callStackSizeLimit,
  callStackSizeLimit_,
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
