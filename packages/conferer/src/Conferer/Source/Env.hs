{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Env where


import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified System.Environment as System
import Data.Maybe (mapMaybe)

import           Conferer.Source
import qualified Conferer.Source.InMemory as InMemory

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
fromConfig :: Prefix -> SourceCreator
fromConfig prefix _config = do
  fromEnv prefix

fromEnv :: Prefix -> IO Source
fromEnv prefix = do
  rawEnvironment <- System.getEnvironment
  return $ fromEnvList rawEnvironment prefix

-- | 'SourceCreator' for env 'Source' that allows parameterizing the
-- function used to lookup for testing
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

envVarToKey :: Prefix -> Text -> Maybe Key
envVarToKey prefix envVar =
  let
    splitEnvVar = fromText $ Text.replace "_"  "." envVar
  in
    keyPrefixOf (fromText prefix) splitEnvVar

