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
  , KeyFragment
  -- * Creating 'Key's
  , mkKey
  , fromText
  -- * Maniputaing 'Key's
  , (/.)
  , stripKeyPrefix
  , isKeyPrefixOf

  , mkKeyFragment
  , isKeyFragment

  , isKeyCharacter
  , mkKeyCharacter
  -- * Introspecting 'Key's
  , rawKeyComponents
  , unconsKey
  ) where

import Conferer.Key.Internal
