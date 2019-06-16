module Conferer.Provider.Dhall where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Dhall
import           Dhall.JSON
import           Conferer.Provider.JSON
import           Conferer.Provider.Files

import           Conferer.Types

mkDhallConfigProvider :: ProviderCreator
mkDhallConfigProvider config = do
  filePath <- getFilePathFromEnv config "dhall"
  fileContent <- Text.readFile filePath
  dahllExpr <- inputExpr fileContent
  case dhallToJSON dahllExpr of
    Right jsonConfig -> mkJsonConfigProvider' jsonConfig config
    Left compileError -> error (show compileError)
