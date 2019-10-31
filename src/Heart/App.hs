module Heart.App
  ( App (..)
  , appLogAction
  , appStore
  , appEnv
  , newApp
  , AppM
  , AppC
  , runAppM
  ) where

import Colog.Actions (richMessageAction)
import Colog.Message (Message)
import Heart.Logging (HasSimpleLog (..))
import Heart.Prelude
import Heart.RIO (RIO, runRIO)
import Heart.Stats (HasStore (..), Store, newStore)

data App env = App
  { _appLogAction :: !(LogAction IO Message)
  , _appStore     :: !Store
  , _appEnv       :: env
  }

$(makeLenses ''App)

instance HasSimpleLog (App env) where
  simpleLogL = appLogAction

instance HasStore (App env) where
  storeL = appStore

newApp :: MonadIO m => env -> m (App env)
newApp env = do
  store <- newStore
  pure (App richMessageAction store env)

type AppM env a = RIO (App env) a

type AppC env m = (HasSimpleLog env, HasStore env, MonadReader env m, MonadThrow m, MonadUnliftIO m, HasCallStack)

runAppM :: env -> AppM env a -> IO a
runAppM env m = do
  app <- newApp env
  runRIO app m
