{-# LANGUAGE ExistentialQuantification #-}

module Heart.Alloc
  ( Alloc
  , newAlloc
  , newEnumAlloc
  , incAlloc
  ) where

import Heart.Prelude
import UnliftIO.IORef

data Alloc e = forall x. Alloc
  { _allocStep :: x -> x
  , _allocRef:: IORef x
  , _allocExtract :: x -> e
  }

newAlloc :: MonadIO m => (e -> e) -> e -> m (Alloc e)
newAlloc f e = fmap (\r -> Alloc f r id) (newIORef e)

newEnumAlloc :: (MonadIO m, Enum e) => m (Alloc e)
newEnumAlloc = newAlloc succ (toEnum 0)

incAlloc :: MonadIO m => Alloc e -> m e
incAlloc (Alloc step ref extract) = atomicModifyIORef' ref (\e -> (step e, extract e))
