module Heart.MultiMap
  ( HashMultiMap
  , insertHashMultiMap
  , invertHashMapWith
  ) where

import Heart.Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type HashMultiMap k v = HashMap k (HashSet v)

insertHashMultiMap :: (Eq k, Hashable k, Eq v, Hashable v) => k -> v -> HashMultiMap k v -> HashMultiMap k v
insertHashMultiMap k v m =
  HM.insert k (maybe (HS.singleton v) (HS.insert v) (HM.lookup k m)) m

invertHashMapWith :: (Monad m, Eq k, Hashable k, Eq u, Hashable u) => (v -> m u) -> HashMap k v -> m (HashMultiMap u k)
invertHashMapWith f m = foldM go HM.empty (HM.toList m) where
  go acc (k, v) = fmap (\u -> insertHashMultiMap u k acc) (f v)
