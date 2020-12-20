module Conferer.Source.Yaml where

import           Data.Yaml

import qualified Conferer.Source.JSON as JSON
import           Conferer.Source.Files
import           Conferer.Source

fromConfig :: SourceCreator
fromConfig config = do
  filePath <- getFilePathFromEnv config "yaml"
  fromFilePath filePath


fromFilePath :: FilePath -> IO Source
fromFilePath filePath = do
  configAsJson <- decodeFileEither filePath
  case configAsJson of
    Right jsonConfig -> return $ JSON.fromValue jsonConfig
    Left parseException -> error (show parseException)
