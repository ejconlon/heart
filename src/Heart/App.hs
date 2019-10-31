module Heart.App where

import Colog.Actions (richMessageAction)
import Colog.Core.Action (LogAction (..))
import Colog.Core.Class (HasLog (..))
import Colog.Message (Message)
import Heart.Prelude
import Heart.Stats (HasStore (..), Store, newStore)

data App m = App
  { _appLogAction :: !(LogAction m Message)
  , _appStore     :: !Store
  }

$(makeLenses ''App)

instance HasLog (App m) Message m where
  getLogAction = _appLogAction
  setLogAction act env = env { _appLogAction = act }
  logActionL = appLogAction

instance HasStore (App m) where
  storeL = appStore

newApp :: MonadIO m => m (App m)
newApp = App richMessageAction <$> newStore

type WithMessageLog env m = WithLog env Message m
