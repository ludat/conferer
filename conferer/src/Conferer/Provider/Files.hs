module Conferer.Provider.Files where

import qualified Data.Text as Text

import           Conferer.Types
import           Conferer.FetchFromConfig.Basics ()

fromRight :: a -> Either e a -> a
fromRight a (Left _) = a
fromRight _ (Right a) = a

getFilePathFromEnv :: Config -> String -> IO FilePath
getFilePathFromEnv config extension = do
  env <- fromRight "dev" <$> fetch "env" config
  return $ mconcat
    [ "config/"
    , Text.unpack env
    , "."
    , extension
    ]
