module Conferer.Source.Null
  (
    -- * Does Nothing Source
    -- | The stub source that never has a key
    mkNullSource
  )
where

import           Conferer.Source

data NullSource = 
  NullSource
  deriving (Show, Eq)

instance IsSource NullSource where
  getKeyInSource _source _key =
    return Nothing
  getSubkeysInSource _source _key =
    return []

-- | Create a null 'Source'
mkNullSource :: SourceCreator
mkNullSource _config =
  return $ Source $ NullSource
