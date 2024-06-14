{-# LANGUAGE TypeApplications #-}
-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public and stable API for the most basic usage of this library
module Conferer
  (

  -- * How to use this doc
  -- | This doc is mostly for reference, so you probably won't learn how to
  --   use conferer by reading it. For more detailed and guided documentation
  --   the best place is the webpage: <https://conferer.ludat.io/docs>

  -- * Creating a Config
  mkConfig
  , mkConfig'
  -- * Getting values from a config
  -- | These functions allow you to get any type that implements 'FromConfig'
  , fetch
  , fetch'
  , fetchKey
  , fetchFromConfig
  , safeFetchKey
  , unsafeFetchKey
  , DefaultConfig(..)
  , FromConfig
  -- * Some useful types
  , Config
  , Key
  ) where

import Data.Text (Text)
import Data.Typeable (Typeable)

import Conferer.Config.Internal
import Conferer.Config.Internal.Types
import Conferer.FromConfig.Internal
import Conferer.Key
import qualified Conferer.Source.Env as Env
import qualified Conferer.Source.CLIArgs as Cli
import qualified Conferer.Source.PropertiesFile as PropertiesFile
import Conferer.Config (Defaults)
import Conferer.FromConfig.Internal.Types
import Control.Exception
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs, getProgName)

-- | Use the 'FromConfig' instance to get a value of type @a@ from the config
--   using some default fallback. The most common use for this is creating a custom
--   record and using this function to fetch it at initialization time.
--
--   This function throws only parsing exceptions when the values are present
--   but malformed somehow (@"abc"@ as an Int) but that depends on the 'FromConfig'
--   implementation for the type.
fetch :: forall a. (FromConfig a, Typeable a, DefaultConfig a, HasExplanation a) => Config -> IO a
fetch c = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["help"] -> do
      undefined
    ["explain"] -> do
      explainKey @a "" c
      exitSuccess
    ["explain", key] -> do
      explainKey @a (mkKey key) c
      exitSuccess
    _ -> do
      fetch' c configDef

-- | Same as 'fetch' but it accepts the default as a parameter instead of using
--   the default from 'configDef'
fetch' :: forall a. (FromConfig a, Typeable a) => Config -> a -> IO a
fetch' c a = do
  asTopLevel $ fetchFromRootConfigWithDefault c a

-- | Given an IO action, it runs and if it throws a Conferer related exception
-- it pretty prints the error and exits the program with failure.
asTopLevel :: IO a -> IO a
asTopLevel action =
  action
     `catch` (\(e :: MissingRequiredKey) -> do
       putStrLn $ displayException e
       exitFailure)
     `catch` (\(e :: ConfigParsingError) -> do
       putStrLn $ displayException e
       exitFailure)

-- | Same as 'fetch'' but you can specify a 'Key' instead of the root key which allows
--   you to fetch smaller values when you need them instead of a big one at
--   initialization time.
fetchKey :: forall a. (FromConfig a, Typeable a) => Config -> Key -> a -> IO a
fetchKey = fetchFromConfigWithDefault

-- | Same as 'fetchKey' but it returns a 'Nothing' when the value isn't present
safeFetchKey :: forall a. (FromConfig a, Typeable a) => Config -> Key -> IO (Maybe a)
safeFetchKey c k = fetchFromConfig k c

-- | Same as 'fetchKey' but it throws when the value isn't present.
unsafeFetchKey :: forall a. (FromConfig a, Typeable a) => Config -> Key -> IO a
unsafeFetchKey c k = fetchFromConfig k c


-- | Create a 'Config' which reads from command line arguments, env vars and
--   property files that depend on the environment (@config/development.properties@)
--   by default
mkConfig :: Text -> IO Config
mkConfig appName =
  pure emptyConfig
  >>= addSource (Cli.fromConfig)
  >>= addSource (Env.fromConfig appName)
  >>= addSource (PropertiesFile.fromConfig "config.file")

-- | Create a 'Config' with the given defaults and source creators.
--   The sources will take precedence by the order they have in the list (earlier in
--   the list means it's tried first).
--   If the requested key is not found in any source it'll be looked up in the defaults.
mkConfig' :: Defaults -> [SourceCreator] -> IO Config
mkConfig' defaults sources = addSources sources . addDefaults defaults $ emptyConfig
