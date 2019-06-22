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


mkEnvProvider :: Prefix -> ProviderCreator
mkEnvProvider prefix =
  mkEnvProvider' System.lookupEnv prefix

mkEnvProvider' :: LookupEnvFunc -> Prefix -> ProviderCreator
mkEnvProvider' lookupEnv prefix = \_config ->
  return $
  Provider
  { getKeyInProvider = \k -> do
      let envVarName = Text.unpack $ keyToEnvVar prefix k
      fmap Text.pack <$> lookupEnv envVarName
  }
