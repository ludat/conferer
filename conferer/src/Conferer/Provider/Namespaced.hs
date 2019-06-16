module Conferer.Provider.Namespaced where

import           Data.List (stripPrefix)

import           Conferer.Types

mkNamespacedProvider :: Key -> ProviderCreator -> ProviderCreator
mkNamespacedProvider (Path key) configCreator = \config -> do
  configProvider <- configCreator config
  return $ Provider
    { getKeyInProvider = \(Path k) -> do
        case stripPrefix key k of
          Just newKey -> getKeyInProvider configProvider (Path newKey)
          Nothing -> return Nothing
    }
