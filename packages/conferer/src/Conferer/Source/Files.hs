-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public API module providing some helper functions related to files
{-# LANGUAGE TypeApplications #-}
module Conferer.Source.Files where

import Data.Maybe (fromMaybe)
import System.FilePath

import Conferer.Config
import Conferer.FromConfig

-- | Helper function to get a file from the config specifying the extension and using
-- current env
getFilePathFromEnv :: Key -> String -> Config -> IO FilePath
getFilePathFromEnv key extension config = do
  env <- fromMaybe "development" <$> fetchFromConfig @(Maybe String) "env" config
  let defaultPath = "config" </> env <.> extension
  File filepath <- fetchFromConfigWithDefault @File config key $ File defaultPath
  return filepath
