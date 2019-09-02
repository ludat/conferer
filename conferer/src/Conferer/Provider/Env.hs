module Conferer.Provider.Env
  (
-- * Env Provider
-- | This 'Provider' provides config values from env vars given a prefix that's
-- used to avoid colliding with different system configuration
--
-- For example if you use the 'Prefix' "awesomeapp" and get the 'Key'
-- "warp.port" this provider will try to lookup the env var called
-- @AWESOMEAPP_WARP_PORT@.

-- * Usage
-- | To use this provider simply choose a prefix and add it using the
-- 'addProvider' function like:
--
-- @
-- config & 'addProvider' ('mkEnvProvider' "awesomeapp")
-- @
    mkEnvProvider
    , mkEnvProvider'
    , Prefix
    , LookupEnvFunc
    , keyToEnvVar
  ) where


import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Environment as System

import           Conferer.Types

-- | 'ProviderCreator' for env 'Provider' that uses the real 'System.lookupEnv'
-- function
mkEnvProvider :: Prefix -> ProviderCreator
mkEnvProvider prefix =
  mkEnvProvider' System.lookupEnv prefix

-- | 'ProviderCreator' for env 'Provider' that allows parameterizing the
-- function used to lookup for testing
mkEnvProvider' :: LookupEnvFunc -> Prefix -> ProviderCreator
mkEnvProvider' lookupEnv prefix = \_config ->
  return $
  Provider
  { getKeyInProvider = \k -> do
      let envVarName = Text.unpack $ keyToEnvVar prefix k
      fmap Text.pack <$> lookupEnv envVarName
  }

-- | Type alias for the function to lookup env vars
type LookupEnvFunc = String -> IO (Maybe String)


-- | A text to namespace env vars
type Prefix = Text

-- | Get the env name from a prefix and a key by uppercasing and
-- intercalating underscores
--
-- >>> keyToEnVar "awesomeapp" "warp.port"
-- "AWESOMEAPP_WARP_PORT"
keyToEnvVar :: Prefix -> Key -> Text
keyToEnvVar prefix (Path keys) =
  Text.toUpper
  $ Text.intercalate "_"
  $ filter (/= mempty)
  $ prefix : keys

