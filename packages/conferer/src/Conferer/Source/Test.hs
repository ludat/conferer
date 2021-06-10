-- |
-- Copyright: (c) 2021 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for testing
module Conferer.Source.Test where

import Conferer.Source
import qualified Conferer.Source.InMemory as InMemory
import Data.Map (Map)
import Data.Text (Text)

-- | 'Source' for testing, mainly for checking that given some
-- behavior from 'Source' some 'fetchFromConfig' will behave
-- properly.
--
-- Note: Don't use this source for real code, use 'InMemorySource'
-- instead, since this source can't explain things to the user
data TestSource =
  TestSource
  { rawMap :: InMemory.RawInMemorySource
  } deriving (Show, Eq)

instance IsSource TestSource where
  getKeyInSource TestSource {..} key =
    return $ InMemory.lookupKey key rawMap
  getSubkeysInSource TestSource {..} key = do
    return $ InMemory.subKeys key rawMap
  explainNotFound _ _ =
    error "Trying to explain a Test Source, use an InMemory source instead"
  explainSettedKey _ _ =
    error "Trying to explain a Test Source, use an InMemory source instead"

-- | Create a 'SourceCreator' from a list of associations
fromConfig :: [(Key, Text)] -> SourceCreator
fromConfig configMap _config =
  return $ fromAssociations configMap

-- | Create a 'Source' from a 'Map'
fromMap :: Map Key Text -> Source
fromMap =
  Source . TestSource . InMemory.rawFromMap

-- | Create a 'Source' from a list of associations
fromAssociations :: [(Key, Text)] -> Source
fromAssociations =
  Source . TestSource . InMemory.rawFromAssociations
