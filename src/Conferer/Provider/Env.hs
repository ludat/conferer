module Conferer.Provider.Env where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Environment as System

import           Conferer.Types


type LookupEnvFunc = (String -> IO (Maybe String))


keyToEnvVar :: Prefix -> Key -> Text
keyToEnvVar prefix (Path keys) =
  Text.toUpper
  $ Text.intercalate "_"
  $ filter (/= mempty)
  $ prefix : keys


mkEnvConfigProvider :: Prefix -> ProviderCreator
mkEnvConfigProvider prefix =
  mkEnvConfigProvider' System.lookupEnv prefix

mkEnvConfigProvider' :: LookupEnvFunc -> Prefix -> ProviderCreator
mkEnvConfigProvider' lookupEnv prefix = \config ->
  return $
  ConfigProvider
  { getKeyInProvider = \k -> do
      let envVarName = Text.unpack $ keyToEnvVar prefix k
      fmap Text.pack <$> lookupEnv envVarName
  }
