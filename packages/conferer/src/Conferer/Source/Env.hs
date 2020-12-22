-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Environment based source
{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Env where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Environment as System
import Data.Maybe (mapMaybe)

import Conferer.Source
import qualified Conferer.Source.InMemory as InMemory

-- | Source that interfaces with the environment transforming keys
-- by uppercasing and interspersing underscores, and using a prefix
-- to avoid clashing with system env vars
--
-- so with "app" prefix, @"some.key"@ turns into @APP_SOME_KEY@
data EnvSource =
  EnvSource
  { environment :: RawEnvironment
  , keyPrefix :: Prefix
  , innerSource :: Source
  } deriving (Show)

-- | Type alias for the environment
type RawEnvironment = [(String, String)]

-- | Type alias for the env vars prefix
type Prefix = Text

instance IsSource EnvSource where
  getKeyInSource EnvSource{..} key = do
    getKeyInSource innerSource key
  getSubkeysInSource EnvSource{..} key = do
    getSubkeysInSource innerSource key

-- | Create a 'SourceCreator' using 'fromEnv'
fromConfig :: Prefix -> SourceCreator
fromConfig prefix _config = do
  fromEnv prefix

-- | Create a 'Source' using the real environment
fromEnv :: Prefix -> IO Source
fromEnv prefix = do
  rawEnvironment <- System.getEnvironment
  return $ fromEnvList rawEnvironment prefix

-- | Create a 'Source' using a hardcoded list of env vars
fromEnvList :: RawEnvironment -> Prefix -> Source
fromEnvList environment keyPrefix =
  let
    mappings =
      mapMaybe (\(key, v) -> do
        k <- envVarToKey keyPrefix $ Text.pack key
        return (k, Text.pack v)
      )
      environment
    innerSource = InMemory.fromAssociations mappings
  in Source EnvSource {..}

-- | Get the env name from a prefix and a key by uppercasing and
-- intercalating underscores
keyToEnvVar :: Prefix -> Key -> Text
keyToEnvVar prefix keys =
  Text.toUpper
  $ Text.intercalate "_"
  $ filter (/= mempty)
  $ prefix : rawKeyComponents keys

-- | The opossite of 'keyToEnvVar'
envVarToKey :: Prefix -> Text -> Maybe Key
envVarToKey prefix envVar =
  let
    splitEnvVar = fromText $ Text.replace "_"  "." envVar
  in
    stripKeyPrefix (fromText prefix) splitEnvVar

