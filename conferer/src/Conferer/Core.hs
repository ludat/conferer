module Conferer.Core where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable, Proxy(..), typeRep)
import           Control.Exception (throw)

import           Conferer.Provider.Simple
import           Conferer.Types

-- | Most Basic function to interact directly with a 'Config'. It always returns
--   'Text' in the case of success and implements the logic to traverse
--   providers inside the 'Config'.
getKey :: Key -> Config -> IO (Maybe Text)
getKey k config =
  go $ providers config ++ [mkPureMapProvider (defaults config)]
  where
    go [] = return Nothing
    go (provider:providers) = do
      res <- getKeyInProvider provider k
      case res of
        Just t -> return $ Just t
        Nothing -> go providers


-- | Fetch a value from a config key that's parsed using the FetchFromConfig
--   instance.
--
--   This function throws an exception if the key is not found.
getFromConfig :: forall a. (Typeable a, FetchFromConfig a) => Key -> Config -> IO a
getFromConfig key config =
  fromMaybe (throw $ FailedToFetchError key (typeRep (Proxy :: Proxy a)))
    <$> fetch key config

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

-- | Instantiate a 'ProviderCreator' using the 'emptyConfig'
mkStandaloneProvider :: ProviderCreator -> IO Provider
mkStandaloneProvider mkProvider =
  mkProvider emptyConfig


-- | Instantiate a 'Provider' using an 'ProviderCretor' and a 'Config' and add
--   to the config
addProvider :: ProviderCreator -> Config -> IO Config
addProvider mkProvider config = do
  newProvider <- mkProvider config
  return $
    config
    { providers = providers config ++ [ newProvider ]
    }

-- | Same as 'getKey' but it throws if the 'Key' isn't found
unsafeGetKey :: Key -> Config -> IO Text
unsafeGetKey key config =
  fromMaybe (throw $ FailedToFetchError key (typeRep (Proxy :: Proxy Text)))
    <$> getKey key config
