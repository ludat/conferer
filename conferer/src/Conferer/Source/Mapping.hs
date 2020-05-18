module Conferer.Source.Mapping
  (
    -- * Namespaced higher-order source
    -- | This source takes a source and returns a new source that
    -- always transforms the key according to either a function for
    -- 'mkMappingSource'' or a 'Map' for 'mkMappingSource'
    mkMappingSource
    , mkMappingSource'
  )
where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Conferer.Types

-- | Create a 'SourceCreator' using a function to transform the supplied keys
-- and another 'SourceCreator'
mkMappingSource' :: (Key -> Maybe Key) -> SourceCreator -> SourceCreator
mkMappingSource' mapper sourceCreator config = do
  configSource <- sourceCreator config

  return $ Source
    { getKeyInSource = \k -> do
        case mapper k of
          Just newKey -> getKeyInSource configSource newKey
          Nothing -> return Nothing
    }

-- | Create a 'SourceCreator' using a 'Map' 'Key' 'Key' to transform the supplied keys
-- and another 'SourceCreator'
mkMappingSource :: Map Key Key -> SourceCreator -> SourceCreator
mkMappingSource configMap configSource =
  mkMappingSource' (`Map.lookup` configMap) configSource
