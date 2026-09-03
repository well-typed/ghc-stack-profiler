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
  SourceLocationDef (..),
  CallStackFrame (..),
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

  -- ** Dehydrate
  dehydrateCallStack,

  -- *** Symbol Table
  SymbolTableWriter (..),
  emptyMapSymbolTableWriter,
  MapTable,
  getKnownStrings,
  getKnownSourceLocations,
) where

import GHC.Stack.Profiler.Core.Internal.CallStack (
  CallStack (..),
  SourceLocation (..),
  StackItem (..),
 )
import GHC.Stack.Profiler.Core.Internal.Dehydrate (
  MapTable,
  SymbolTableWriter (..),
  dehydrateCallStack,
  emptyMapSymbolTableWriter,
  getKnownSourceLocations,
  getKnownStrings,
 )
import GHC.Stack.Profiler.Core.Internal.Eventlog (
  CallStackChunk (..),
  CallStackFrame (..),
  CapabilityId (..),
  IpeId (..),
  Message (..),
  SourceLocationDef (..),
  SourceLocationId (..),
  StringDef (..),
  StringId (..),
  ThreadId (..),
  deserializeEventlogMessage,
  joinCallStackChunks,
 )
import GHC.Stack.Profiler.Core.Internal.Hydrate (
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
