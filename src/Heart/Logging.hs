module Heart.Logging
  ( WithMessageLog
  ) where

import Heart.Prelude

type WithMessageLog env m = WithLog env Message m
