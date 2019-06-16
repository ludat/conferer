module Conferer.Provider.Files where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Either (fromRight)

import           Conferer.Types
import           Conferer.FetchFromConfig.Basics ()
import           Conferer.Core

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- fromRight "dev" <$> fetch "env" config
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
