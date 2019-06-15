module Conferer.Provider.Files where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))

import           Conferer.Types
import           Conferer.Core

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- unsafeGetKey "env" config
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
