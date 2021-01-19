-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for dhall config files
module Conferer.Source.Dhall where

import qualified Data.Text.IO as Text
import Dhall
import Dhall.JSON
import System.Directory (doesFileExist)
import Conferer.Source.Files
import qualified Conferer.Source.Aeson as JSON
import qualified Conferer.Source.Null as Null

import Conferer.Source
import Control.Exception

-- | Create a 'SourceCreator' from a dhall config file
-- using 'fromFilePath'
fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "dhall" config
  fromFilePath' filePath


-- | Create a 'SourceCreator' from a filepath reading it as dhall
-- if the file doesn't exist do nothing, but if it has invalid
-- dhall throw an exception.
fromFilePath :: FilePath -> SourceCreator
fromFilePath filePath _config =
  fromFilePath' filePath

-- | Create a 'Source' from a filepath reading it as dhall
-- if the file doesn't exist do nothing, but if it has invalid
-- dhall throw an exception.
fromFilePath' :: FilePath -> IO Source
fromFilePath' filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      fileContent <- Text.readFile filePath
      dhallExpr <- inputExpr fileContent
      case dhallToJSON dhallExpr of
        Right jsonConfig -> do
          return $ JSON.fromValue jsonConfig
        Left compileError ->
          throwIO $ ErrorCall (show compileError)
    else do
      return Null.empty
