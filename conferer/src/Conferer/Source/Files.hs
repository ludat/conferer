{-# LANGUAGE TypeApplications #-}
module Conferer.Source.Files where

import qualified Data.Text as Text
import Data.Text (Text)

import           Conferer.Config
import           Conferer.FromConfig

fromRight :: a -> Either e a -> a
fromRight a (Left _) = a
fromRight _ (Right a) = a

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- getFromConfigWithDefault @Text "env" config "development"
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
