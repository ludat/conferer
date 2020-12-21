module Conferer.Config.Internal.Types where

import Data.Map (Map)
import Conferer.Key (Key)
import Data.Dynamic ( Dynamic )
import Conferer.Source.Internal (Source)
import Data.Text (Text)
-- | Core type that the user of this library interact with, in the future it may
--   contain more this besides a list of sources
data Config =
  Config
  { configSources :: [Source]
  , configDefaults :: Map Key Dynamic
  , configKeyMappings :: [(Key, Key)]
  } deriving (Show)

data KeyLookupResult
  = MissingKey [Key]
  | FoundInSources Key Text
  | FoundInDefaults Key Dynamic
  deriving (Show)

-- | The type for creating a source given a 'Config', some sources require a
-- certain configuration to be initialized (for example: the redis source
-- needs connection info to connect to the server)
type SourceCreator = Config -> IO Source