module Heart.Prelude
  ( module Prelude
  , FromJSON (..)
  , Generic
  , HasLog (..)
  , Int64
  , Lens'
  , LogAction (..)
  , Message
  , MonadIO (..)
  , MonadReader (..)
  , MonadThrow (..)
  , MonadTrans (..)
  , MonadUnliftIO (..)
  , Proxy (..)
  , Rep
  , Seq (..)
  , Severity (..)
  , Text
  , ToJSON (..)
  , UnliftIO (..)
  , WithLog
  , log
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  , makeLenses
  , makePrisms
  , over
  , set
  , view
  ) where

import Colog.Core.Action (LogAction (..))
import Colog.Core.Class (HasLog (..))
import Colog.Core.Severity (Severity (..))
import Colog.Message (Message, log, logDebug, logError, logException, logInfo, logWarning)
import Colog.Monad (WithLog)
import Control.Lens (Lens', over, set, view)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Prelude hiding (log)
