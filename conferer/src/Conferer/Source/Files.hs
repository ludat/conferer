{-# LANGUAGE TypeApplications #-}
module Conferer.Source.Files where

import Data.Text (Text)
import qualified Data.Text as Text

import Conferer.Config
import Conferer.FromConfig

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- fetchFromConfigWithDefault @Text "env" config "development"
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
