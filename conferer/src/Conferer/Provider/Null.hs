module Conferer.Provider.Null where

import           Conferer.Types

mkNullProvider :: ProviderCreator
mkNullProvider _config =
  return $ Provider
  { getKeyInProvider =
      \k -> do
        return Nothing
  }
