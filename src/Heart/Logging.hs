module Heart.Logging
  ( WithMessageLog
  ) where

import Heart.Prelude

type WithMessageLog env = WithLog env Message IO
