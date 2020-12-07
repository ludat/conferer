{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Env where


import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified System.Environment as System
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
  getKeyInSource EnvSource{..} key = do
    getKeyInSource innerSource key
  getSubkeysInSource EnvSource{..} key = do
    getSubkeysInSource innerSource key

-- | 'SourceCreator' for env 'Source' that uses the real 'System.lookupEnv'
-- function
mkEnvSource :: Prefix -> SourceCreator
mkEnvSource prefix config = do
  rawEnvironment <- System.getEnvironment
  mkEnvSource' rawEnvironment prefix config

-- | 'SourceCreator' for env 'Source' that allows parameterizing the
-- function used to lookup for testing
mkEnvSource' :: RawEnvironment -> Prefix -> SourceCreator
mkEnvSource' environment keyPrefix config = do
  let
    mappings =
      mapMaybe (\(key, v) -> do
        k <- envVarToKey keyPrefix $ Text.pack key
        return (k, Text.pack v)
      )
      environment
  innerSource <- mkMapSource mappings config
  return $
    Source $
      EnvSource {..}

-- | Get the env name from a prefix and a key by uppercasing and
-- intercalating underscores
keyToEnvVar :: Prefix -> Key -> Text
keyToEnvVar prefix keys =
  Text.toUpper
  $ Text.intercalate "_"
  $ filter (/= mempty)
  $ prefix : rawKeyComponents keys

envVarToKey :: Prefix -> Text -> Maybe Key
envVarToKey prefix envVar =
  let
    splitEnvVar = fromText $ Text.replace "_"  "." envVar
  in
    keyPrefixOf (fromText prefix) splitEnvVar

