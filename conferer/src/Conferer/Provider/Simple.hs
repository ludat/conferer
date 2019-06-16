module Conferer.Provider.Simple where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import           Conferer.Types

mkMapProvider' :: Map Key Text -> ProviderCreator
mkMapProvider' configMap _config =
  return $ Provider
    { getKeyInProvider =
      \k -> do
        return $ Map.lookup k configMap
    }

mkMapProvider :: [(Key, Text)] -> ProviderCreator
mkMapProvider = mkMapProvider' . Map.fromList
