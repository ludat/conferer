module Conferer.Source.Null
  (
    -- * Does Nothing Source
    -- | The stub source that never has a key
    mkNullSource
  )
where

import           Conferer.Types

-- | Create a null 'Source'
mkNullSource :: SourceCreator
mkNullSource _config =
  return $ Source
  { getKeyInSource =
      \_k -> do
        return Nothing
  }
