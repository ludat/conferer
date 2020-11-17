module Conferer.Source.Yaml where

import           Data.Yaml

import           Conferer.Source.JSON
import           Conferer.Source.Files
import           Conferer.Source

mkYamlSource :: SourceCreator
mkYamlSource config = do
  filePath <- getFilePathFromEnv config "yaml"
  configAsJson <- decodeFileEither filePath
  case configAsJson of
    Right jsonConfig -> mkJsonSource' jsonConfig config
    Left parseException -> error (show parseException)
