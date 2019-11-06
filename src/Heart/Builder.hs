{-# LANGUAGE ExistentialQuantification #-}

module Heart.Builder
  ( Builder
  , premapBuilder
  , newBuilder
  , monoidBuilder
  , addBuilder
  , addAllBuilder
  , consumeBuilder
  , drainBuilder
  , runBuilder
  , evalBuilder
  ) where

import Heart.Prelude
import qualified ListT
import UnliftIO.IORef

data Builder a b = forall x. Builder
  { _builderStep :: x -> a -> x
  , _builderRef :: IORef x
  , _builderExtract :: x -> b
  }

instance Functor (Builder a) where
  fmap f (Builder step ref extract) = Builder step ref (f . extract)

premapBuilder :: (z -> a) -> Builder a b -> Builder z b
premapBuilder f (Builder step ref extract) = Builder (\x z -> step x (f z)) ref extract

newBuilder :: MonadIO m => (b -> a -> b) -> b -> m (Builder a b)
newBuilder f b = fmap (\ref -> Builder f ref id) (newIORef b)

monoidBuilder :: (MonadIO m, Monoid b) => m (Builder b b)
monoidBuilder = newBuilder mappend mempty

addBuilder :: MonadIO m => Builder a b -> a -> m ()
addBuilder (Builder step ref _) b = modifyIORef' ref (`step` b)

addAllBuilder :: (MonadIO m, Foldable f) => Builder a b -> f a -> m ()
addAllBuilder (Builder step ref _) as = modifyIORef' ref (\a -> foldl step a as)

consumeBuilder :: MonadIO m => Builder a b -> ListT m a -> m ()
consumeBuilder b = ListT.traverse_ (addBuilder b)

drainBuilder :: MonadIO m => Builder a b -> m b
drainBuilder (Builder _ ref extract) = fmap extract (readIORef ref)

runBuilder :: MonadIO m => (b -> a -> b) -> b -> (Builder a b -> m c) -> m (b, c)
runBuilder f a h = do
  s <- newBuilder f a
  c <- h s
  a' <- drainBuilder s
  pure (a', c)

evalBuilder :: MonadIO m => (b -> a -> b) -> b -> (Builder a b -> m ()) -> m b
evalBuilder f a h = fst <$> runBuilder f a h
