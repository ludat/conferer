-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public API for Key related features
module Conferer.Key
  ( Key
  -- * Creating 'Key's
  , mkKey
  , fromText
  -- * Maniputaing 'Key's
  , (/.)
  , stripKeyPrefix
  , isKeyPrefixOf
  , isValidKeyFragment
  , isKeyCharacter
  -- * Introspecting 'Key's
  , rawKeyComponents
  , unconsKey
  ) where

import Conferer.Key.Internal
