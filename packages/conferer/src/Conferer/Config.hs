-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public API providing Config functionality
module Conferer.Config
  (
    -- * Data
    Config
    -- * Querying a config
  , getKey
  , KeyLookupResult(..)
  , listSubkeys
    -- * Config creation and initialization
  , emptyConfig
  , addSource
  , addDefault
  , addDefaults
  , addKeyMappings
    -- * Re-Exports
  , module Conferer.Key
  , (&)
  ) where

import Conferer.Config.Internal
import Conferer.Config.Internal.Types
import Conferer.Key
import Data.Function
