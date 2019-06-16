module Conferer.Provider.Dhall where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Dhall
import           Dhall.JSON
import           Conferer.Provider.JSON
import           Conferer.Provider.Files

import           Conferer.Types

mkDhallProvider :: ProviderCreator
mkDhallProvider config = do
  filePath <- getFilePathFromEnv config "dhall"
  fileContent <- Text.readFile filePath
  dahllExpr <- inputExpr fileContent
  case dhallToJSON dahllExpr of
    Right jsonConfig -> mkJsonProvider' jsonConfig config
    Left compileError -> error (show compileError)
