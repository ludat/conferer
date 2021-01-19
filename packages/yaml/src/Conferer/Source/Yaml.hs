-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for json config files using Aeson
module Conferer.Source.Yaml where

import Data.Yaml

import qualified Conferer.Source.Aeson as JSON
import Conferer.Source.Files
import Conferer.Source

-- | Create a 'SourceCreator' from a yaml file that we get from the env
-- with the same logic as 'JSON.JSONSource'
fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "yaml" config
  fromFilePath' filePath

-- | Create a 'SourceCreator' by reading the provided path as json
fromFilePath :: FilePath -> SourceCreator
fromFilePath filePath _config =
  fromFilePath' filePath

-- | Create a 'Source' by reading the provided path as json 
fromFilePath' :: FilePath -> IO Source
fromFilePath' filePath = do
  configAsJson <- decodeFileEither filePath
  case configAsJson of
    Right jsonConfig -> return $ JSON.fromValue jsonConfig
    Left parseException -> error (show parseException)
