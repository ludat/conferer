module Conferer.Provider.Simple where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import           Conferer.Types

mkMapConfigProvider' :: Map Key Text -> ProviderCreator
mkMapConfigProvider' configMap _config =
  return $ ConfigProvider
  { getKeyInProvider =
      \k -> do
        return $ Map.lookup k configMap
  }

mkMapConfigProvider :: [(Key, Text)] -> ProviderCreator
mkMapConfigProvider = mkMapConfigProvider' . Map.fromList
