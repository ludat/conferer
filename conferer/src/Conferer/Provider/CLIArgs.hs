module Conferer.Provider.CLIArgs where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import           System.Environment (getArgs)

import Conferer.Types
import Conferer.Provider.Simple


mkCLIArgsProvider' :: [String] -> ProviderCreator
mkCLIArgsProvider' args = \config -> do
  let configMap = parseArgsIntoKeyValue args
  mkMapProvider configMap config

mkCLIArgsProvider :: ProviderCreator
mkCLIArgsProvider = \config -> do
  args <- getArgs
  mkCLIArgsProvider' args config

parseArgsIntoKeyValue :: [String] -> [(Key, Text)]
parseArgsIntoKeyValue =
  fmap (\(k, s) -> (fromString $ Text.unpack k, s)) .
  fmap (\s -> fmap (Text.drop 1) $ Text.breakOn "=" s).
  mapMaybe (Text.stripPrefix "--") .
  takeWhile (/= "--") .
  fmap Text.pack
