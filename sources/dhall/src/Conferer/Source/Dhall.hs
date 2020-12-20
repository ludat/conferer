module Conferer.Source.Dhall where

import qualified Data.Text.IO as Text
import Dhall
import Dhall.Core
import Data.Void
import Dhall.JSON
import System.Directory (doesFileExist)
import Conferer.Source.Files
import qualified Conferer.Source.JSON as JSON
import qualified Conferer.Source.Null as Null

import Conferer.Source

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
      return $ fromDhallExpr dhallExpr
    else do
      return Null.empty

fromDhallExpr :: Expr s Void -> Source
fromDhallExpr dhallExpr =
  case dhallToJSON dhallExpr of
    Right jsonConfig -> do
      JSON.fromValue jsonConfig
    Left compileError ->
      error (show compileError)
