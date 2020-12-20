{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.InMemory
  (
    -- * Simple Source
    -- | This source provides values from a hardcoded Map passed at creation
    -- time that can not be changed afterwards, it's mostly used as a necessary
    -- utility
    SimpleSource(..)
  , fromConfig
  , fromAssociations
  , fromMap
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Conferer.Source

newtype SimpleSource = 
  SimpleSource
  { configMap :: Map Key Text
  } deriving (Show, Eq)

instance IsSource SimpleSource where
  getKeyInSource SimpleSource {..} key =
    return $ Map.lookup key configMap
  getSubkeysInSource SimpleSource {..} key = do
    return $ filter (\k -> key `isKeyPrefixOf` k && key /= k) $ Map.keys configMap

fromConfig :: [(Key, Text)] -> SourceCreator
fromConfig configMap _config =
  return $ fromAssociations configMap

-- | Make a 'SourceCreator' from a 'Map'
fromMap :: Map Key Text -> Source
fromMap configMap =
  Source . SimpleSource $ configMap

-- | Make a 'Source' from 'List' of 'Key', 'Text' pairs
fromAssociations :: [(Key, Text)] -> Source
fromAssociations =
  fromMap . Map.fromList
