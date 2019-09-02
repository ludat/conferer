module Conferer.Provider.Null
  (
    -- * Does Nothing Provider
    -- | The stub provider that never has a key
    mkNullProvider
  )
where

import           Conferer.Types

-- | Create a null 'Provider'
mkNullProvider :: ProviderCreator
mkNullProvider _config =
  return $ Provider
  { getKeyInProvider =
      \_k -> do
        return Nothing
  }
