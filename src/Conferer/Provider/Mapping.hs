module Conferer.Provider.Mapping where

import           Data.List (stripPrefix)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Conferer.Types

mkMappingProvider' :: (Key -> Maybe Key) -> ProviderCreator -> ProviderCreator
mkMappingProvider' mapper providerCreator config = do
  configProvider <- providerCreator config

  return $ ConfigProvider
    { getKeyInProvider = \k -> do
        case mapper k of
          Just newKey -> getKeyInProvider configProvider newKey
          Nothing -> return Nothing
    }

mkMappingProvider :: Map Key Key -> ProviderCreator -> ProviderCreator
mkMappingProvider configMap configProvider =
  mkMappingProvider' (`Map.lookup` configMap) configProvider
