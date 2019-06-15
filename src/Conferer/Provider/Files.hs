module Conferer.Provider.Files where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Either (fromRight)

import           Conferer.Types
import           Conferer.Core

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- fromRight "dev" <$> getKey "env" config
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
