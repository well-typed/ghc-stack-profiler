module GHC.Stack.Profiler.Internal.Util (
  castPtrToWord64,

  -- * Glob Patterns
  Glob,
  matches,

  -- * DList
  DList,

  -- * WriterT
  WriterT,
  tell,
  runWriterT,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.String (IsString (..))
import Data.Word
import Foreign.Ptr
import GHC.IsList (IsList (..))

castPtrToWord64 :: Ptr a -> Word64
castPtrToWord64 ptr = case ptrToWordPtr ptr of
  WordPtr w -> fromIntegral w -- On platforms that use 32-bit systems, the key is still Word64

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

-- | A glob pattern.
--
--   Use `fromString` to construct glob patterns from strings.
--
--   A @*@ matches any string, including the empty string.
--
--   One can remove the special meaning of @*@ by preceding it with a backslash.
newtype Glob = Glob [GlobPart]

data GlobPart = Wildcard | Literal String

instance IsString Glob where
  fromString :: String -> Glob
  fromString = Glob . go
   where
    go :: String -> [GlobPart]
    go [] = []
    go ('*' : pat) = Wildcard : go pat
    go ('\\' : '*' : pat) = literal ['*'] (go pat)
    go (c : pat) = literal [c] (go pat)

    literal :: String -> [GlobPart] -> [GlobPart]
    literal lit (Literal lit' : pat) = Literal (lit <> lit') : pat
    literal lit pat = Literal lit : pat

instance Show Glob where
  showsPrec :: Int -> Glob -> ShowS
  showsPrec p (Glob pat) = showsPrec p (go pat)
   where
    go [] = []
    go (Wildcard : pat') = '*' : go pat'
    go (Literal lit : pat') = escape lit <> go pat'

    escape :: String -> String
    escape [] = []
    escape ('*' : str) = '\\' : '*' : escape str
    escape (c : str) = c : escape str

-- | Test if the glob pattern matches the given string.
matches :: Glob -> String -> Bool
matches (Glob parts) = go parts
 where
  go [] _str = True
  go [Wildcard] _str = True
  go (Wildcard : pat'@(Wildcard : _)) str = go pat' str
  go (Wildcard : Literal lit : pat') str = any (go pat') (skipWildcardLiteral lit str)
  go (Literal lit : pat') str = maybe False (go pat') (skipPrefix lit str)

  -- Stream the possible remainders after matching a wildcard followed by a literal.
  --
  -- NOTE: O( n * m ) where n = length str and m = length lit.
  skipWildcardLiteral :: String -> String -> [String]
  skipWildcardLiteral _lit [] = []
  skipWildcardLiteral lit str@(_c : str')
    -- NOTE: yield suff, but continue searching from str', in case of overlaps.
    | Just suff <- skipPrefix lit str = suff : skipWildcardLiteral lit str'
    | otherwise = skipWildcardLiteral lit str'

  -- Stream the possible remainders after matching a literal.
  --
  -- NOTE: O( m ) where m = length lit
  skipPrefix :: String -> String -> Maybe String
  skipPrefix [] str = Just str
  skipPrefix (_ : _) [] = Nothing
  skipPrefix (l : lit') (c : str') = if l == c then skipPrefix lit' str' else Nothing

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

newtype DList a = MkDList {unDList :: [a] -> [a]}

instance Semigroup (DList a) where
  (<>) :: DList a -> DList a -> DList a
  MkDList xs <> MkDList ys = MkDList (xs . ys)
  {-# INLINE (<>) #-}

instance Monoid (DList a) where
  mempty :: DList a
  mempty = MkDList id
  {-# INLINE mempty #-}

instance IsList (DList a) where
  type Item (DList a) = a

  toList :: DList a -> [a]
  toList = ($ []) . unDList
  {-# INLINE toList #-}

  fromList :: [a] -> DList a
  fromList = MkDList . (++)
  {-# INLINE fromList #-}

-------------------------------------------------------------------------------
-- WriterT
-------------------------------------------------------------------------------

newtype WriterT w m a = WriterT {unWriterT :: w -> m (a, w)}

instance (Functor m) => Functor (WriterT w m) where
  fmap :: (Functor m) => (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f m = WriterT $ \w -> (\(a, w') -> (f a, w')) <$> unWriterT m w
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (WriterT w m) where
  pure ::
    (Functor m, Monad m) =>
    a -> WriterT w m a
  pure a = WriterT $ \w -> return (a, w)
  {-# INLINE pure #-}

  (<*>) ::
    (Functor m, Monad m) =>
    WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
  WriterT mf <*> WriterT mx = WriterT $ \w -> do
    (f, w') <- mf w
    (x, w'') <- mx w'
    return (f x, w'')
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (WriterT w m) where
  (>>=) ::
    (Monad m) =>
    WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  m >>= k = WriterT $ \w -> do
    (a, w') <- unWriterT m w
    unWriterT (k a) w'
  {-# INLINE (>>=) #-}

writer :: (Monoid w, Monad m) => (a, w) -> WriterT w m a
writer (a, w') = WriterT $ \w ->
  let wt = w `mappend` w' in wt `seq` return (a, wt)
{-# INLINE writer #-}

tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = writer ((), w)
{-# INLINE tell #-}

runWriterT :: (Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = unWriterT m mempty
{-# INLINE runWriterT #-}

lift :: (Monad m) => m a -> WriterT w m a
lift m = WriterT $ \w -> do
  a <- m
  return (a, w)
{-# INLINE lift #-}

instance (MonadIO m) => MonadIO (WriterT w m) where
  liftIO :: (MonadIO m) => IO a -> WriterT w m a
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
