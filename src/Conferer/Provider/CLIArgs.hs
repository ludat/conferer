module Conferer.Provider.CLIArgs where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)

import Conferer.Types
import Conferer.Provider.Simple


mkCLIArgsProvider' :: [String] -> ProviderCreator
mkCLIArgsProvider' args = \config -> do
  let configMap = parseArgsIntoKeyValue args
  mkMapConfigProvider configMap config

mkCLIArgsProvider :: ProviderCreator
mkCLIArgsProvider = undefined

parseArgsIntoKeyValue :: [String] -> [(Key, Text)]
parseArgsIntoKeyValue =
        fmap (\(k, s) -> (fromString $ Text.unpack k, s)) .
        fmap (\s -> if elem '=' $ Text.unpack s
                then Text.breakOn "=" s
                else (s, "true")) .
        mapMaybe (Text.stripPrefix "-X") .
        fmap Text.pack
