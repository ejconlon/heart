module Heart.RIO
  ( RIO (..)
  , runRIO
  ) where

import Control.Monad.IO.Unlift (withUnliftIO)
import Control.Monad.Reader (ReaderT (..))
import Heart.Prelude

newtype RIO r a = RIO { unRIO :: ReaderT r IO a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadThrow)

instance MonadUnliftIO (RIO env) where
  askUnliftIO = RIO (ReaderT (\r -> withUnliftIO (\u -> return (UnliftIO (unliftIO u . flip runReaderT r . unRIO)))))

runRIO :: MonadIO m => r -> RIO r a -> m a
runRIO r m = liftIO (runReaderT (unRIO m) r)
