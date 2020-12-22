-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public API module providing FromConfig functionality
module Conferer.FromConfig 
  ( FromConfig(fetchFromConfig)
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

  , ConfigParsingError
  , throwConfigParsingError
  , configParsingError

  , TypeMismatchWithDefault
  , throwTypeMismatchWithDefault
  , typeMismatchWithDefault
  , Key
  , (/.)
  , File(..)
  , KeyLookupResult(..)

  , fetchFromDefaults
  , fetchRequiredFromDefaults
  ) where

import Conferer.FromConfig.Internal
import Conferer.Config.Internal.Types
import Conferer.Key