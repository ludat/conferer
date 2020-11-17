module Conferer.Source.Dhall where

import qualified Data.Text.IO as Text
import           Dhall
import           Dhall.JSON
import           System.Directory (doesFileExist)
import           Conferer.Source.JSON
import           Conferer.Source.Files
import           Conferer.Source.Null

import           Conferer.Source

mkDhallSource :: SourceCreator
mkDhallSource config = do
  filePath <- getFilePathFromEnv config "dhall"
  fileExists <- doesFileExist filePath
  if fileExists
    then do
    fileContent <- Text.readFile filePath
    dahllExpr <- inputExpr fileContent
    case dhallToJSON dahllExpr of
      Right jsonConfig -> do
        mkJsonSource' jsonConfig config
      Left compileError -> error (show compileError)
    else do
      mkNullSource config
