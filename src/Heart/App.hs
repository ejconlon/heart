module Heart.App where

import Colog.Actions (richMessageAction)
import Colog.Message (Message)
import Control.Monad.Reader (ReaderT (..))
import Heart.Prelude
import Heart.Stats (HasStore (..), Store, newStore)

data App (m :: * -> *) = App
  { _appLogAction :: !(LogAction m Message)
  , _appStore     :: !Store
  }

$(makeLenses ''App)

instance HasLog (App m) Message m where
  getLogAction = view logActionL
  setLogAction = set logActionL
  logActionL = appLogAction

instance HasStore (App m) where
  storeL = appStore

newApp :: MonadIO m => m (App m)
newApp = App richMessageAction <$> newStore

data MainEnv (r :: *) (m :: * -> *) = MainEnv
  { _mainApp :: App m
  , _mainData :: r
  }

$(makeLenses ''MainEnv)

newMainEnv :: MonadIO m => r -> m (MainEnv r m)
newMainEnv r = flip MainEnv r <$> newApp

newtype Main r m a = Main { unMain :: ReaderT (MainEnv r m) m a }
  deriving (Functor, Applicative, Monad, MonadReader (MainEnv r m), MonadIO, MonadThrow, MonadCatch)

instance MonadUnliftIO m => MonadUnliftIO (Main r m) where
    askUnliftIO = Main (fmap (\(UnliftIO run) -> UnliftIO (run . unMain)) askUnliftIO)
    withRunInIO go = Main (withRunInIO (\k -> go (k . unMain)))

instance HasLog (MainEnv r m) Message m where
  getLogAction = view logActionL
  setLogAction = set logActionL
  logActionL = mainApp . appLogAction

instance HasStore (MainEnv r m) where
  storeL = mainApp . appStore

runMain :: MonadIO m => Main r m a -> r -> m a
runMain m r = do
  e <- newMainEnv r
  runReaderT (unMain m) e
