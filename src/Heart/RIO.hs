module Heart.RIO
  ( RIO (..)
  , runRIO
  ) where

import Control.Monad.IO.Unlift (withUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Heart.Prelude

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO, MonadThrow)

instance MonadUnliftIO (RIO env) where
  askUnliftIO = RIO (ReaderT (\r -> withUnliftIO (\u -> return (UnliftIO (unliftIO u . flip runReaderT r . unRIO)))))

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO r m = liftIO (runReaderT (unRIO m) r)
