-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public API module providing FromConfig functionality
module Conferer.FromConfig
  ( FromConfig(fromConfig)
  , fetchFromConfig
  , DefaultConfig(configDef)
  , fetchFromConfigWithDefault
  , fetchFromRootConfig
  , fetchFromRootConfigWithDefault

  , fetchFromConfigByIsString
  , fetchFromConfigByRead
  , fetchFromConfigWith
  , addDefaultsAfterDeconstructingToDefaults

  , MissingRequiredKey
  , throwMissingRequiredKey
  , missingRequiredKey
  , missingRequiredKeys

  , ConfigParsingError
  , throwConfigParsingError
  , configParsingError

  , Key
  , (/.)
  , File(..)
  , KeyLookupResult(..)
  , LookupTarget(..)
  , OverrideFromConfig(..)
  , overrideFetch
  , getKeyFromSources
  , getKeyFromDefaults
  , getKey

  , NotUserConfigurable(..)
  , unwrapNotConfigurable
  ) where

import Conferer.FromConfig.Internal
import Conferer.FromConfig.Internal.Types
import Conferer.Config.Internal.Types
import Conferer.Config.Internal
import Conferer.Key
