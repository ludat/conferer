module Conferer.Provider.Namespaced
  (
    -- * Namespaced higher-order provider
    -- | This provider takes a provider and returns a new provider that
    -- always checks that the 'Key' given always starts with certain 'Key'
    -- and then strips that prefix before consulting its inner Provider
    mkNamespacedProvider
  ) where

import           Data.List (stripPrefix)

import           Conferer.Types

-- | Create a 'ProviderCreator' from a prefix and another 'ProviderCreator'
mkNamespacedProvider :: Key -> ProviderCreator -> ProviderCreator
mkNamespacedProvider (Path key) configCreator = \config -> do
  configProvider <- configCreator config
  return $ Provider
    { getKeyInProvider = \(Path k) -> do
        case stripPrefix key k of
          Just newKey -> getKeyInProvider configProvider (Path newKey)
          Nothing -> return Nothing
    }
