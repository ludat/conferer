{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Env
  (
-- * Env Source
-- | This 'Source' provides config values from env vars given a prefix that's
-- used to avoid colliding with different system configuration
--
-- For example if you use the 'Prefix' "awesomeapp" and get the 'Key'
-- "warp.port" this source will try to lookup the env var called
-- @AWESOMEAPP_WARP_PORT@.

-- * Usage
-- | To use this source simply choose a prefix and add it using the
-- 'addSource' function like:
--
-- @
-- config & 'addSource' ('mkEnvSource' "awesomeapp")
-- @
    mkEnvSource
    , mkEnvSource'
    , Prefix
    , Environment
    , keyToEnvVar
  ) where


import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified System.Environment as System
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)

import           Conferer.Source
import           Conferer.Source.Simple (mkMapSource)

data EnvSource =
  EnvSource
  { environment :: RawEnvironment
  , keyPrefix :: Prefix
  , innerSource :: Source
  } deriving (Show)

-- | Type alias for the function to lookup env vars
type Environment = Map String String

-- | Type alias for the function to lookup env vars
type RawEnvironment = [(String, String)]

-- | A text to namespace env vars
type Prefix = Text

instance IsSource EnvSource where
  getKeyInSource (EnvSource {..}) key = do
    getKeyInSource innerSource key
  getSubkeysInSource (EnvSource {..}) key = do
    getSubkeysInSource innerSource key

-- | 'SourceCreator' for env 'Source' that uses the real 'System.lookupEnv'
-- function
mkEnvSource :: Prefix -> SourceCreator
mkEnvSource prefix = \config -> do
  rawEnvironment <- System.getEnvironment
  mkEnvSource' rawEnvironment prefix config

-- | 'SourceCreator' for env 'Source' that allows parameterizing the
-- function used to lookup for testing
mkEnvSource' :: RawEnvironment -> Prefix -> SourceCreator
mkEnvSource' environment keyPrefix = \config -> do
  let
    mappings =
      mapMaybe (\(key, v) -> do
        k <- envVarToKey keyPrefix key
        return (k, v)
      ) $
      fmap (\(k, v) -> (Text.pack k, Text.pack v)) $
      environment
  innerSource <- mkMapSource mappings config
  return $
    Source $
      EnvSource {..}

-- | Get the env name from a prefix and a key by uppercasing and
-- intercalating underscores
keyToEnvVar :: Prefix -> Key -> Text
keyToEnvVar prefix (Path keys) =
  Text.toUpper
  $ Text.intercalate "_"
  $ filter (/= mempty)
  $ prefix : keys

envVarToKey :: Prefix -> Text -> Maybe Key
envVarToKey prefix' envVar =
  let
    prefix = Text.toLower prefix'
    splitEnvVar = Text.splitOn "_" $ Text.toLower envVar
  in
    case stripPrefix [prefix] splitEnvVar of
      Just restOfKey ->
        Just $ Path restOfKey
      Nothing ->
        Nothing

