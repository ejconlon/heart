module Heart.UnionFind
  ( Partition
  , PartitionAlloc
  , Term
  , newPartitionAlloc
  , newTerm
  , findTermEx
  , findTerm
  , unionTerm
  , decideEqTerm
  , splitClasses
  ) where

import Data.Function (on)
import Heart.Alloc
import Heart.MultiMap
import Heart.Prelude
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

-- The s type param a phantom to allow the caller to ensure that partitions
-- of different "things" can't be mixed up.
newtype Partition s = Partition { unPartition :: Int } deriving (Eq, Ord, Enum, Show, Hashable)

-- From Content/Term encoding simplified from
--- https://github.com/ekmett/guanxi/blob/master/src/Equality.hs
data Content s
  = Root !Int
  | Child !(Term s)

data Term s = Term
  { _termPartition :: !(Partition s)
  , _termParent :: !(IORef (Content s))
  }

instance Eq (Term s) where
  (==) = (==) `on` _termPartition

instance Ord (Term s) where
  (<=) = (<=) `on` _termPartition

instance Hashable (Term s) where
  hash = hash . _termPartition
  hashWithSalt d = hashWithSalt d . _termPartition

newtype PartitionAlloc s = PartitionAlloc { unPartitionAlloc :: Alloc (Partition s) }

newPartitionAlloc :: MonadIO m => m (PartitionAlloc s)
newPartitionAlloc = fmap PartitionAlloc newEnumAlloc

newTerm :: MonadIO m => PartitionAlloc s -> m (Term s)
newTerm (PartitionAlloc alloc) = do
  p <- incAlloc alloc
  r <- newIORef (Root 0)
  pure (Term p r)

findTermEx :: MonadIO m => Term s -> m (Int, Term s)
findTermEx t@(Term _ r) = do
  y <- readIORef r
  case y of
    Root i -> pure (i, t)
    Child s -> do
      z <- findTermEx s
      let (_, q) = z
      z <$ writeIORef r (Child q)

findTerm :: MonadIO m => Term s -> m (Term s)
findTerm = fmap snd . findTermEx

unionTerm :: MonadIO m => Term s -> Term s -> m ()
unionTerm m n = do
  (mrank, mroot) <- findTermEx m
  (nrank, nroot) <- findTermEx n
  let mref = _termParent mroot
      nref = _termParent nroot
  case compare mrank nrank of
    LT -> do
      writeIORef mref (Child nroot)
      writeIORef nref (Root nrank)
    GT -> do
      writeIORef nref (Child mroot)
      writeIORef mref (Root mrank)
    EQ -> do
      writeIORef mref (Child nroot)
      writeIORef nref (Root (nrank + 1))

decideEqTerm :: MonadIO m => Term s -> Term s -> m Bool
decideEqTerm m n = (==) <$> findTerm m <*> findTerm n

splitClasses :: (Eq k, Hashable k, MonadIO m) => HashMap k (Term s) -> m (HashMultiMap (Term s) k)
splitClasses = invertHashMapWith findTerm
