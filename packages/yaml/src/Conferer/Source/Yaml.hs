module Conferer.Source.Yaml where

import           Data.Yaml

import qualified Conferer.Source.Aeson as JSON
import           Conferer.Source.Files
import           Conferer.Source

fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "yaml" config
  fromFilePath filePath


fromFilePath :: FilePath -> IO Source
fromFilePath filePath = do
  configAsJson <- decodeFileEither filePath
  case configAsJson of
    Right jsonConfig -> return $ JSON.fromValue jsonConfig
    Left parseException -> error (show parseException)
