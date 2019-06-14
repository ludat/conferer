module Conferer.Provider.Namespaced where

import           Data.List (stripPrefix)

import           Conferer.Types

mkNamespacedProvider :: Key -> ConfigProvider -> ConfigProvider
mkNamespacedProvider (Path key) configProvider =
  ConfigProvider
  { getKeyInProvider = \(Path k) -> do
      case stripPrefix key k of
        Just newKey -> getKeyInProvider configProvider (Path newKey)
        Nothing -> return Nothing
  }
