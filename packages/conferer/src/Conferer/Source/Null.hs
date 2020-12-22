-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Do nothing source
module Conferer.Source.Null where

import Conferer.Source

-- | Stub source that has no keys
data NullSource =
  NullSource
  deriving (Show, Eq)

instance IsSource NullSource where
  getKeyInSource _source _key =
    return Nothing
  getSubkeysInSource _source _key =
    return []

-- | Create 'SourceCreator'
fromConfig :: SourceCreator
fromConfig _config =
  return empty

-- | Create a 'Source'
empty :: Source
empty =
  Source $ NullSource
