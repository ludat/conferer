module Conferer.Provider.Yaml where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as B
import           Data.Yaml
import           Conferer.Provider.JSON
import           Conferer.Provider.Files

import           Conferer.Types

mkYamlConfigProvider :: ProviderCreator
mkYamlConfigProvider config = do
  filePath <- getFilePathFromEnv config "yaml"
  configAsJson <- decodeFileEither filePath
  case configAsJson of
    Right jsonConfig -> mkJsonConfigProvider' jsonConfig config
    Left parseException -> error (show parseException)
