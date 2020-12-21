{-# LANGUAGE TypeApplications #-}
module Conferer.Source.Files where

import Data.Maybe (fromMaybe)
import System.FilePath

import Conferer.Config
import Conferer.FromConfig

getFilePathFromEnv :: Key -> String -> Config -> IO FilePath
getFilePathFromEnv key extension config = do
  env <- fromMaybe "development" <$> fetchFromConfig @(Maybe String) "env" config
  let defaultPath = "config" </> env <.> extension
  File filepath <- fetchFromConfigWithDefault @File key config $ File $ defaultPath
  return filepath
