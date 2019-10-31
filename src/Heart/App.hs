module Heart.App where

import Colog.Actions (richMessageAction)
import Colog.Message (Message)
import Control.Monad.Reader (ReaderT (..))
import Heart.Logging (HasSimpleLog (..))
import Heart.Prelude
import Heart.Stats (HasStore (..), Store, newStore)

data App = App
  { _appLogAction :: !(LogAction IO Message)
  , _appStore     :: !Store
  }

$(makeLenses ''App)

instance HasSimpleLog App where
  simpleLogL = appLogAction

instance HasStore App where
  storeL = appStore

newApp :: IO App
newApp = App richMessageAction <$> newStore

data MainEnv (r :: *) = MainEnv
  { _mainApp :: App
  , _mainData :: r
  }

$(makeLenses ''MainEnv)

newMainEnv :: r -> IO (MainEnv r)
newMainEnv r = flip MainEnv r <$> newApp

newtype Main r a = Main { unMain :: ReaderT (MainEnv r) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (MainEnv r), MonadIO, MonadThrow)

instance MonadUnliftIO (Main r) where
    askUnliftIO = Main $ do
      e <- ask
      pure (UnliftIO (\m -> runReaderT (unMain m) e))

instance HasSimpleLog (MainEnv r) where
  simpleLogL = mainApp . appLogAction

instance HasStore (MainEnv r) where
  storeL = mainApp . appStore

runMain :: Main r a -> r -> IO a
runMain m r = do
  e <- newMainEnv r
  runReaderT (unMain m) e
