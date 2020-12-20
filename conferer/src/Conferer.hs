-- |
-- Module:      Conferer
-- Copyright:   (c) 2019 Lucas David Traverso
-- License:     MIT
-- Maintainer:  Lucas David Traverso <lucas6246@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for managing configuration effectively
module Conferer
  (

  -- * How to use this library
  -- | This is the most basic example: which uses the default configuration
  --   to get a configuration for warp, which can be overriden via env vars,
  --   command line arguments of @.properties@ files
  --
  -- @
  -- import Conferer
  -- import Conferer.FromConfig.Warp () -- from package conferer-warp
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   warpSettings <- 'fetchFromConfig' \"warp\" config
  --   runSettings warpSettings application
  -- @
  --
  -- In the above example we see that we are getting a configuration value for
  -- warp under the key warp, so for example to override it's default value
  -- provided by warp the config keys for warp will always look like
  -- @warp.something@, for example to override the port for warp (3000 by
  -- default) we could call our program as @./my_program --warp.port=8000@.
  --
  -- There are two sides to conferer: Getting configuration for other libraries
  -- like warp, hspec, snap, etc. and the way we choose to provide values like
  -- json files, properties files, env vars, etc.

  -- ** Getting configuration for existing libraries
  -- | There is a typeclass 'FromConfig' that defines how to get a type
  --   from a config, they are implemented in different packages since the
  --   weight of the dependencies would be too high, the package is usually
  --   named as @conferer-DEPENDENCY@ where DEPENDENCY is the name of the dependency (
  --   for example: conferer-snap, conferer-warp, conferer-hspec), if you find
  --   a library without a conferer port for its config you can create an issue
  --   or maybe even create the library yourself!

  -- ** Providing key value pairs for configuration
  -- | There is one important type in conferer: 'Config' from which, given a key
  -- (eg: @warp@) you can get anything that implements 'FromConfig' (like
  -- 'Warp.Settings')
  --
  -- Internally a 'Config' is made of many 'Source's which have a simpler
  -- interface:
  --
  -- @
  -- 'getKeyInSource' :: Source -> Key -> IO (Maybe Text)
  -- @
  --
  -- Most configuration sources can be abstracted away as Map String String,
  -- and they can use whatever logic they want to turn conferer keys (a list of
  -- strings) into a place to look for a string, (for example the env source
  -- requires a string to namespace the env vars that can affect the
  -- configuration)
  --
  -- Once you have your 'Source' you can add it to a 'Config' using the
  -- 'addSource' function. One final note: each source has a different
  -- priority, which depends on when is was added to the config ('Source's
  -- added later have lower priority) so the config searches keys in sources
  -- in the same order they were added.

  mkConfig
  , Config
  , DefaultConfig(..)
  , FromConfig(..)
  , fetch
  , fetchKey
  , unsafeFetch
  , unsafeFetchKey
  , Key
  ) where

import Data.Text (Text)

import Conferer.Config.Internal
import Conferer.Config.Internal.Types
import Conferer.FromConfig.Internal
import Conferer.Key
import qualified Conferer.Source.Env as Env
import qualified Conferer.Source.CLIArgs as Cli
import qualified Conferer.Source.PropertiesFile as PropertiesFile
import Data.Typeable (Typeable)

fetch :: forall a. (FromConfig a, Typeable a) => Config -> a -> IO a
fetch = fetchFromRootConfigWithDefault

fetchKey :: forall a. (FromConfig a, Typeable a) => Key -> Config -> a -> IO a
fetchKey = fetchFromConfigWithDefault

unsafeFetch :: forall a. (FromConfig a) => Config -> IO a
unsafeFetch = fetchFromRootConfig

unsafeFetchKey :: forall a. (FromConfig a) => Key -> Config -> IO a
unsafeFetchKey = fetchFromConfig

-- | Default config which reads from command line arguments, env vars and
-- property files
mkConfig :: Text -> IO Config
mkConfig appName =
  pure emptyConfig
  >>= addSource (Cli.fromConfig)
  >>= addSource (Env.fromConfig appName)
  >>= addSource (PropertiesFile.fromConfig)
