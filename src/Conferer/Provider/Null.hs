module Conferer.Provider.Null where

import           Conferer.Types

mkNullProvider :: ProviderCreator
mkNullProvider _config =
  return $ ConfigProvider
  { getKeyInProvider =
      \k -> do
        return Nothing
  }
