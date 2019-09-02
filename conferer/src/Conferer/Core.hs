module Conferer.Core where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Either (either)

import           Conferer.Types

-- | Most Basic function to interact directly with a 'Config'. It always returns
--   'Text' in the case of success and implements the logic to traverse
--   providers inside the 'Config'.
getKey :: Key -> Config -> IO (Either Text Text)
getKey k config =
  go $ providers config
  where
    go [] = return $ Left ("Key '" `Text.append` keyName k `Text.append` "' was not found")
    go (provider:providers) = do
      res <- getKeyInProvider provider k
      case res of
        Just t -> return $ Right t
        Nothing -> go providers


-- | Fetch a value from a config key that's parsed using the FetchFromConfig
--   instance.
--
--   This function throws an exception if the key is not found.
getFromConfig :: FetchFromConfig a => Key -> Config -> IO a
getFromConfig k config =
  either (error . Text.unpack) id <$> fetch k config

-- | Create a new 'Key' by concatenating two existing keys.
(/.) :: Key -> Key -> Key
parent /. child = Path (unKey parent ++ unKey child)

-- | The empty configuration, this 'Config' is used as the base for
--   most config creating functions.
emptyConfig :: Config
emptyConfig = Config []

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
    Config
    { providers = providers config ++ [ newProvider ]
    }

-- | Same as 'getKey' but it throws if the 'Key' isn't found
unsafeGetKey :: Key -> Config -> IO Text
unsafeGetKey k config =
  either (error . Text.unpack) id <$> getKey k config
