module Conferer.Provider.Dhall where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Dhall
import           Dhall.JSON
import           System.Directory (doesFileExist)
import           Conferer.Provider.JSON
import           Conferer.Provider.Files
import           Conferer.Provider.Null

import           Conferer.Types

mkDhallProvider :: ProviderCreator
mkDhallProvider config = do
  filePath <- getFilePathFromEnv config "dhall"
  fileExists <- doesFileExist filePath
  if fileExists
    then do
    fileContent <- Text.readFile filePath
    dahllExpr <- inputExpr fileContent
    case dhallToJSON dahllExpr of
      Right jsonConfig -> do
        mkJsonProvider' jsonConfig config
      Left compileError -> error (show compileError)
    else do
      mkNullProvider config
