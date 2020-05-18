{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Conferer.Core where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable, Proxy(..), typeRep)
import           Control.Exception (try, throw, throwIO, evaluate)

import           Conferer.Source.Simple
import           Conferer.Types

-- | Most Basic function to interact directly with a 'Config'. It always returns
--   'Text' in the case of success and implements the logic to traverse
--   sources inside the 'Config'.
getKey :: Key -> Config -> IO (Maybe Text)
getKey k config =
  go $ sources config ++ [mkPureMapSource (defaults config)]
  where
    go [] = return Nothing
    go (source:sources) = do
      res <- getKeyInSource source k
      case res of
        Just t -> return $ Just t
        Nothing -> go sources


-- | Fetch a value from a config under some specific key that's parsed using the 'FromConfig'
--   instance, and as a default it uses the value from 'DefaultConfig'.
--
--   Notes:
--     - This function may throw an exception if parsing fails for any subkey
getFromConfig :: forall a. (Typeable a, FromConfig a, DefaultConfig a) => Key -> Config -> IO a
getFromConfig key config =
  getFromConfigWithDefault key config configDef

-- | Same as 'getFromConfig' using the root key
--
--   Notes:
--     - This function may throw an exception if parsing fails for any subkey
getFromRootConfig :: forall a. (Typeable a, FromConfig a, DefaultConfig a) => Config -> IO a
getFromRootConfig config =
  getFromConfig "" config


-- | Same as 'getFromConfig' but with a user defined default (instead of 'DefaultConfig' instance)
--
--   Useful for fetching primitive types
getFromConfigWithDefault :: forall a. (Typeable a, FromConfig a) => Key -> Config -> a -> IO a
getFromConfigWithDefault key config configDefault =
  safeGetFromConfigWithDefault key config configDefault
    >>= \case
      Just value -> do
        evaluate value
      Nothing ->
        throwIO $ FailedToFetchError key (typeRep (Proxy :: Proxy a))

-- | Fetch a value from a config key that's parsed using the FromConfig instance.
--
--   Note: This function does not use default so the value must be fully defined by the config only,
--   meaning using this function for many records will always result in 'Nothing' (if the record contains
--   a value that can never be retrieved like a function)
safeGetFromConfig :: forall a. (Typeable a, FromConfig a, DefaultConfig a) => Key -> Config -> IO (Maybe a)
safeGetFromConfig key config =
  safeGetFromConfigWithDefault key config configDef

-- | Same as 'safeGetFromConfig' but with a user defined default
safeGetFromConfigWithDefault :: forall a. (Typeable a, FromConfig a) => Key -> Config -> a -> IO (Maybe a)
safeGetFromConfigWithDefault key config configDefault = do
  totalValue <- evaluate =<< fetchFromConfig key config
  case totalValue of
    Just value -> do
      Just <$> evaluate value
    Nothing -> do
      result :: Either FailedToFetchError a <- try . (evaluate =<<) . updateFromConfig key config $ configDefault
      case result of
        Right a -> Just <$> evaluate a
        Left e -> return Nothing

-- | Create a new 'Key' by concatenating two existing keys.
(/.) :: Key -> Key -> Key
parent /. child = Path (unKey parent ++ unKey child)

-- | The empty configuration, this 'Config' is used as the base for
--   most config creating functions.
emptyConfig :: Config
emptyConfig = Config [] Map.empty

withDefaults :: [(Key, Text)] -> Config -> Config
withDefaults configMap config =
  config { defaults = Map.fromList configMap }

-- | Instantiate a 'SourceCreator' using the 'emptyConfig'
mkStandaloneSource :: SourceCreator -> IO Source
mkStandaloneSource mkSource =
  mkSource emptyConfig


-- | Instantiate a 'Source' using an 'SourceCretor' and a 'Config' and add
--   to the config
addSource :: SourceCreator -> Config -> IO Config
addSource mkSource config = do
  newSource <- mkSource config
  return $
    config
    { sources = sources config ++ [ newSource ]
    }

-- | Same as 'getKey' but it throws if the 'Key' isn't found
unsafeGetKey :: Key -> Config -> IO Text
unsafeGetKey key config =
  fromMaybe (throw $ FailedToFetchError key (typeRep (Proxy :: Proxy Text)))
    <$> getKey key config
