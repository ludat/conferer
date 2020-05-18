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
    , LookupEnvFunc
    , keyToEnvVar
  ) where


import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Environment as System

import           Conferer.Types

-- | 'SourceCreator' for env 'Source' that uses the real 'System.lookupEnv'
-- function
mkEnvSource :: Prefix -> SourceCreator
mkEnvSource prefix =
  mkEnvSource' System.lookupEnv prefix

-- | 'SourceCreator' for env 'Source' that allows parameterizing the
-- function used to lookup for testing
mkEnvSource' :: LookupEnvFunc -> Prefix -> SourceCreator
mkEnvSource' lookupEnv prefix = \_config ->
  return $
  Source
  { getKeyInSource = \k -> do
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

