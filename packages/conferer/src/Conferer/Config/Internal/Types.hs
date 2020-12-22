-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Core types for Config (here because of depedency cycles)
module Conferer.Config.Internal.Types where

import Data.Map (Map)
import Conferer.Key (Key)
import Data.Dynamic ( Dynamic )
import Conferer.Source.Internal (Source)
import Data.Text (Text)

-- | This type acts as the entry point for most of the library, it's main purpouse
--   is to expose a uniform interface into multiple configuration sources (such as
--   env vars, cli args, and many others including use defined ones using the
--   'Source' interface)
data Config =
  Config
  { configSources :: [Source]
  , configDefaults :: Map Key Dynamic
  , configKeyMappings :: [(Key, Key)]
  } deriving (Show)

-- | Result of a key lookup in a 'Config'
data KeyLookupResult
  = MissingKey [Key]
  | FoundInSources Key Text
  | FoundInDefaults Key Dynamic
  deriving (Show)

-- | The type for creating a source given a 'Config', some sources require a
-- certain configuration to be initialized (for example: the redis source
-- needs connection info to connect to the server)
type SourceCreator = Config -> IO Source