module Heart.Prelude
  ( module Prelude
  , Alternative (..)
  , Coercible
  , FromJSON (..)
  , FromJSONKey
  , Generic
  , Getter
  , HasCallStack
  , Exception (..)
  , Hashable (..)
  , HashMap
  , HashSet
  , Identity (..)
  , Int64
  , IsString
  , Lens'
  , ListT (..)
  , LogAction (..)
  , Map
  , Message
  , MonadFail (..)
  , MonadIO (..)
  , MonadReader (..)
  , MonadThrow (..)
  , MonadTrans (..)
  , MonadUnliftIO (..)
  , Newtype
  , Proxy (..)
  , Rep
  , Seq (..)
  , Set
  , Setter'
  , Severity (..)
  , Text
  , ToJSON (..)
  , ToJSONKey
  , Typeable
  , UnliftIO (..)
  , asum
  , coerce
  , coerced
  , foldl'
  , foldM
  , for
  , for_
  , makeLenses
  , makePrisms
  , over
  , set
  , simple
  , unless
  , view
  , when
  ) where

import Colog.Core.Action (LogAction (..))
import Colog.Core.Severity (Severity (..))
import Colog.Message (Message)
import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..))
import Control.Lens (Getter, Lens', Setter', coerced, over, set, simple, view)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad (foldM, unless, when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Newtype.Generics (Newtype)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (asum, for_)
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Rep)
import GHC.Stack (HasCallStack)
import ListT (ListT (..))
import Prelude hiding (fail, log)
