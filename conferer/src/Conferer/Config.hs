module Conferer.Config
  ( Config
  , addSource
  , addDefault
  , addDefaults
  , withKeyMappings
  , addKeyMappings
  , emptyConfig
  , getKey
  , KeyLookupResult(..)
  , module Conferer.Key
  ) where

import Conferer.Config.Internal
import Conferer.Config.Internal.Types
import Conferer.Key