module Conferer.Provider.Simple
  (
    -- * Simple Provider
    -- | This provider provides values from a hardcoded Map passed at creation
    -- time that can not be changed afterwards, it's mostly used as a necessary
    -- utility
    mkMapProvider
  , mkMapProvider'
  , mkPureMapProvider
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Conferer.Types

-- | Make a 'ProviderCreator' from a 'Map'
mkMapProvider' :: Map Key Text -> ProviderCreator
mkMapProvider' configMap _config =
  return $ mkPureMapProvider configMap

-- | Make a 'Provider' from a 'Map'
mkPureMapProvider :: Map Key Text -> Provider
mkPureMapProvider configMap =
  Provider
    { getKeyInProvider =
      \k -> do
        return $ Map.lookup k configMap
    }

-- | Make a 'Provider' from 'List' of 'Key', 'Text' pairs
mkMapProvider :: [(Key, Text)] -> ProviderCreator
mkMapProvider = mkMapProvider' . Map.fromList
