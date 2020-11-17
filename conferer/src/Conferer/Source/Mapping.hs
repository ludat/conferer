{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Mapping
  (
    -- * Namespaced higher-order source
    -- | This source takes a source and returns a new source that
    -- always transforms the key according to either a function for
    -- 'mkMappingSource'' or a 'Map' for 'mkMappingSource'
    mkMappingSource
    , mkMappingSource'
  )
where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

import           Conferer.Source

data MappingSource =
  MappingSource
  { innerSource :: Source
  , keysMap :: Map Key Key
  , invertedKeysMap :: Map Key Key
  } deriving (Show)

instance IsSource MappingSource where
  getKeyInSource MappingSource {..} key = do
    case Map.lookup key keysMap of
      Just newKey -> getKeyInSource innerSource newKey
      Nothing -> return Nothing
  getSubkeysInSource MappingSource{..} key = do
    case Map.lookup key keysMap of
      Just newKey -> do
        mapMaybe (`Map.lookup` invertedKeysMap) <$> getSubkeysInSource innerSource newKey
      Nothing -> return []

-- | Create a 'SourceCreator' using a function to transform the supplied keys
-- and another 'SourceCreator'
mkMappingSource' :: Map Key Key -> SourceCreator -> SourceCreator
mkMappingSource' keysMap sourceCreator config = do
  innerSource <- sourceCreator config
  let invertedKeysMap = Map.fromList $ fmap swap $ Map.toList $ keysMap

  return $ Source $ 
    MappingSource {..}

-- | Create a 'SourceCreator' using a 'Map' 'Key' 'Key' to transform the supplied keys
-- and another 'SourceCreator'
mkMappingSource :: Map Key Key -> SourceCreator -> SourceCreator
mkMappingSource configMap configSource =
  mkMappingSource' configMap configSource
