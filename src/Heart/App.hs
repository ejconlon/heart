module Heart.App
  ( App (..)
  , appLogAction
  , appStore
  , appData
  , newApp
  , AppM
  , runAppM
  ) where

import Colog.Actions (richMessageAction)
import Colog.Message (Message)
import Heart.Logging (HasSimpleLog (..))
import Heart.Prelude
import Heart.RIO (RIO, runRIO)
import Heart.Stats (HasStore (..), Store, newStore)

data App r = App
  { _appLogAction :: !(LogAction IO Message)
  , _appStore     :: !Store
  , _appData      :: r
  }

$(makeLenses ''App)

instance HasSimpleLog (App r) where
  simpleLogL = appLogAction

instance HasStore (App r) where
  storeL = appStore

newApp :: MonadIO m => r -> m (App r)
newApp r = do
  store <- newStore
  pure (App richMessageAction store r)

type AppM r a = RIO (App r) a

runAppM :: r -> AppM r a -> IO a
runAppM r m = do
  app <- newApp r
  runRIO app m
