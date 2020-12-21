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

fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "dhall" config
  fromFilePath filePath

fromFilePath :: FilePath -> IO Source
fromFilePath filePath = do
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
