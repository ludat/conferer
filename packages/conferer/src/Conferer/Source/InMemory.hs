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

-- | Function to explain what a user may do to set this key.
--
-- For more information about the semantics look as 'explainNotFound'
--
-- Mostly just a newtype to distinguish from 'ExplainSettedKey'
newtype ExplainNotFound = ExplainNotFound (Key -> String)
-- | Function to tell the user to reference this key.
--
-- For more information about the semantics look as 'explainSettedKey'
--
-- Mostly just a newtype to distinguish from 'ExplainNotFound'
newtype ExplainSettedKey = ExplainSettedKey (Key -> String)

-- | A 'Source' mostly use for mocking which is configured directly using a
-- 'Map', is also contains some functions for explaining the user how to set
-- this source's keys, you can check other sources for examples
data InMemorySource =
  InMemorySource
  { innerRawMap :: RawInMemorySource
  , inMemoryExplainNotFound :: Key -> String
  , inMemoryExplainSettedKey :: Key -> String
  }

-- | Newtype for reusing a simple key -> text map for building other 'Source's
newtype RawInMemorySource =
  RawInMemorySource (Map Key Text)
  deriving (Show, Eq)


instance Show InMemorySource where
  show InMemorySource {..} = "InMemorySource " ++ show innerRawMap

instance IsSource InMemorySource where
  getKeyInSource InMemorySource {..} key =
    return $ lookupKey key innerRawMap
  getSubkeysInSource InMemorySource {..} key = do
    return $ subKeys key innerRawMap
  explainNotFound InMemorySource {..} key =
    inMemoryExplainNotFound key
  explainSettedKey InMemorySource {..} key =
    inMemoryExplainSettedKey key

-- | Lookup a 'Key' inside a 'RawInMemorySource', mainly used to use
-- functionality of 'InMemorySource' without using it, to customize
-- the error messages.
lookupKey :: Key -> RawInMemorySource -> Maybe Text
lookupKey key (RawInMemorySource m) =
  Map.lookup key m

-- | List subkeys from a 'Key' inside a 'RawInMemorySource', mainly
-- used to use functionality of 'InMemorySource' without using it,
-- to customize the error messages.
subKeys :: Key -> RawInMemorySource -> [Key]
subKeys key (RawInMemorySource m) =
  filter (\k -> key `isKeyPrefixOf` k && key /= k) $ Map.keys m

-- | Create a 'RawInMemorySource'.
rawFromMap :: Map Key Text -> RawInMemorySource
rawFromMap =
  RawInMemorySource

-- | Same as 'rawFromMap' but provide associations instead of a map.
rawFromAssociations :: [(Key, Text)] -> RawInMemorySource
rawFromAssociations =
  RawInMemorySource . Map.fromList

-- | Create a 'SourceCreator' from a list of associations
fromConfig :: ExplainNotFound -> ExplainSettedKey -> [(Key, Text)] -> SourceCreator
fromConfig notFound setted configMap _config =
  return $ fromAssociations notFound setted configMap

-- | Create a 'Source' from a 'Map'
fromMap :: ExplainNotFound -> ExplainSettedKey -> Map Key Text -> Source
fromMap (ExplainNotFound inMemoryExplainNotFound) (ExplainSettedKey inMemoryExplainSettedKey) configMap =
  let innerRawMap = rawFromMap configMap
  in Source $ InMemorySource {..}

-- | Create a 'Source' from a list of associations
fromAssociations :: ExplainNotFound -> ExplainSettedKey -> [(Key, Text)] -> Source
fromAssociations notFound setted assocs =
  fromMap notFound setted $ Map.fromList assocs
