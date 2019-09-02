module Conferer.Provider.Mapping
  (
    -- * Namespaced higher-order provider
    -- | This provider takes a provider and returns a new provider that
    -- always transforms the key according to either a function for
    -- 'mkMappingProvider'' or a 'Map' for 'mkMappingProvider'
    mkMappingProvider
    , mkMappingProvider'
  )
where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Conferer.Types

-- | Create a 'ProviderCreator' using a function to transform the supplied keys
-- and another 'ProviderCreator'
mkMappingProvider' :: (Key -> Maybe Key) -> ProviderCreator -> ProviderCreator
mkMappingProvider' mapper providerCreator config = do
  configProvider <- providerCreator config

  return $ Provider
    { getKeyInProvider = \k -> do
        case mapper k of
          Just newKey -> getKeyInProvider configProvider newKey
          Nothing -> return Nothing
    }

-- | Create a 'ProviderCreator' using a 'Map' 'Key' 'Key' to transform the supplied keys
-- and another 'ProviderCreator'
mkMappingProvider :: Map Key Key -> ProviderCreator -> ProviderCreator
mkMappingProvider configMap configProvider =
  mkMappingProvider' (`Map.lookup` configMap) configProvider
