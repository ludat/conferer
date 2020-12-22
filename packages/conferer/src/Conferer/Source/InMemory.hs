-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- In memory source mostly used for testing
{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.InMemory where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Conferer.Source

-- | A 'Source' mostly use for mocking which is configured directly using a
-- 'Map'
newtype InMemorySource =
  InMemorySource
  { configMap :: Map Key Text
  } deriving (Show, Eq)

instance IsSource InMemorySource where
  getKeyInSource InMemorySource {..} key =
    return $ Map.lookup key configMap
  getSubkeysInSource InMemorySource {..} key = do
    return $ filter (\k -> key `isKeyPrefixOf` k && key /= k) $ Map.keys configMap

-- | Create a 'SourceCreator' from a list of associations
fromConfig :: [(Key, Text)] -> SourceCreator
fromConfig configMap _config =
  return $ fromAssociations configMap

-- | Create a 'Source' from a 'Map'
fromMap :: Map Key Text -> Source
fromMap configMap =
  Source . InMemorySource $ configMap

-- | Create a 'Source' from a list of associations
fromAssociations :: [(Key, Text)] -> Source
fromAssociations =
  fromMap . Map.fromList
