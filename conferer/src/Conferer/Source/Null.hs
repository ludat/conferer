module Conferer.Source.Null
  (
    -- * Does Nothing Source
    -- | The stub source that never has a key
    fromConfig
    , empty
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
fromConfig :: SourceCreator
fromConfig _config =
  return $ empty

-- | Create a null 'Source'
empty :: Source
empty =
  Source $ NullSource
