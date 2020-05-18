module Conferer.Source.Namespaced
  (
    -- * Namespaced higher-order source
    -- | This source takes a source and returns a new source that
    -- always checks that the 'Key' given always starts with certain 'Key'
    -- and then strips that prefix before consulting its inner Source
    mkNamespacedSource
  ) where

import           Data.List (stripPrefix)

import           Conferer.Types

-- | Create a 'SourceCreator' from a prefix and another 'SourceCreator'
mkNamespacedSource :: Key -> SourceCreator -> SourceCreator
mkNamespacedSource (Path key) configCreator = \config -> do
  configSource <- configCreator config
  return $ Source
    { getKeyInSource = \(Path k) -> do
        case stripPrefix key k of
          Just newKey -> getKeyInSource configSource (Path newKey)
          Nothing -> return Nothing
    }
