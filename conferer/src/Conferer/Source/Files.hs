module Conferer.Source.Files where

import qualified Data.Text as Text
import           Data.Maybe (fromMaybe)

import           Conferer.Types
import           Conferer.FromConfig.Basics ()

fromRight :: a -> Either e a -> a
fromRight a (Left _) = a
fromRight _ (Right a) = a

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- updateFromConfig "env" config "development"
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
