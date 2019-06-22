module Conferer.Provider.Null where

import           Conferer.Types

mkNullProvider :: ProviderCreator
mkNullProvider _config =
  return $ Provider
  { getKeyInProvider =
      \_k -> do
        return Nothing
  }
