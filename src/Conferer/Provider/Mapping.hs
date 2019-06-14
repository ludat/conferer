module Conferer.Provider.Mapping where

import           Data.List (stripPrefix)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Conferer.Types

mkMappingProvider' :: (Key -> Maybe Key) -> ConfigProvider -> ConfigProvider
mkMappingProvider' mapper configProvider =
  ConfigProvider
  { getKeyInProvider = \k -> do
      case mapper k of
        Just newKey -> getKeyInProvider configProvider newKey
        Nothing -> return Nothing
  }

mkMappingProvider :: Map Key Key -> ConfigProvider -> ConfigProvider
mkMappingProvider configMap configProvider =
  mkMappingProvider' (`Map.lookup` configMap) configProvider
