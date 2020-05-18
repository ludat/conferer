module Conferer.Source.Simple
  (
    -- * Simple Source
    -- | This source provides values from a hardcoded Map passed at creation
    -- time that can not be changed afterwards, it's mostly used as a necessary
    -- utility
    mkMapSource
  , mkMapSource'
  , mkPureMapSource
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Conferer.Types

-- | Make a 'SourceCreator' from a 'Map'
mkMapSource' :: Map Key Text -> SourceCreator
mkMapSource' configMap _config =
  return $ mkPureMapSource configMap

-- | Make a 'Source' from a 'Map'
mkPureMapSource :: Map Key Text -> Source
mkPureMapSource configMap =
  Source
    { getKeyInSource =
      \k -> do
        return $ Map.lookup k configMap
    }

-- | Make a 'Source' from 'List' of 'Key', 'Text' pairs
mkMapSource :: [(Key, Text)] -> SourceCreator
mkMapSource = mkMapSource' . Map.fromList
