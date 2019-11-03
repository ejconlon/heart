module Heart.ListT where

import Heart.Prelude
import qualified ListT

effectListT :: Monad m => m (ListT m a) -> ListT m a
effectListT = ListT . (>>= ListT.uncons)

filterListT :: MonadIO m => (a -> m Bool) -> ListT m a -> ListT m a
filterListT pcate values = effectListT $ do
  mu <- ListT.uncons values
  case mu of
    Just (first, rest) -> do
      ok <- pcate first
      let filteredRest = filterListT pcate rest
      if ok
        then pure (ListT.cons first filteredRest)
        else pure filteredRest
    Nothing -> pure empty

seqListT :: Monad m => ListT m a -> m (Seq a)
seqListT = go Empty where
  go ss vs = do
    m <- ListT.uncons vs
    case m of
      Just (s, vs') -> go (ss :|> s) vs'
      Nothing -> pure ss
