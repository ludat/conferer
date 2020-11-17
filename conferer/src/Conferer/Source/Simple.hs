{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Simple
  (
    -- * Simple Source
    -- | This source provides values from a hardcoded Map passed at creation
    -- time that can not be changed afterwards, it's mostly used as a necessary
    -- utility
    mkMapSource
  , mkMapSource'
  , mkPureMapSource
  , SimpleSource(..)
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
    return $ filter (key `isKeyPrefixOf`) $ Map.keys configMap

-- | Make a 'SourceCreator' from a 'Map'
mkMapSource' :: Map Key Text -> SourceCreator
mkMapSource' configMap _config =
  return $ mkPureMapSource configMap

-- | Make a 'Source' from a 'Map'
mkPureMapSource :: Map Key Text -> Source
mkPureMapSource =
  Source . SimpleSource

-- | Make a 'Source' from 'List' of 'Key', 'Text' pairs
mkMapSource :: [(Key, Text)] -> SourceCreator
mkMapSource =
  mkMapSource' . Map.fromList
