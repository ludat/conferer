-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Properties file source
module Conferer.Source.PropertiesFile where

import Data.Text (Text)
import Data.Function ((&))
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Conferer.Source
import Conferer.Source.Files
import qualified Conferer.Source.Null as Null
import qualified Conferer.Source.InMemory as InMemory

-- | 'Source' for properties file 'Source' that read from a
-- config file in @config/{env}.properties@ and parses it as a properties
-- file with @some.key=a value@ lines
data PropertiesFileSource =
  PropertiesFileSource
  { originalFilePath :: FilePath
  , innerSource :: Source
  } deriving (Show)

instance IsSource PropertiesFileSource where
  getKeyInSource PropertiesFileSource{..} key = do
    getKeyInSource innerSource key
  getSubkeysInSource PropertiesFileSource{..} key = do
    getSubkeysInSource innerSource key

-- | Create a 'SourceCreator' using 'getFilePathFromEnv' to get the path to file
-- and 'fromFilePath'
fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "properties" config
  fromFilePath filePath

-- | Create a 'Source' reading the file and using that as a properties file, but
-- if the file doesn't exist do nothing.
fromFilePath :: FilePath -> IO Source
fromFilePath filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      fileContent <- Text.readFile filePath
      return $ fromFileContent filePath fileContent
    else
      return Null.empty

-- | Create a 'Source' using some content as a properties file
fromFileContent :: FilePath -> Text -> Source
fromFileContent originalFilePath fileContent =
  let keyValues =
        fileContent
        & Text.lines
        & fmap lineToKeyValue
        & catMaybes
      innerSource = InMemory.fromAssociations keyValues
  in Source $ PropertiesFileSource {..}


-- | Transform a line into a key/value pair (or not)
lineToKeyValue :: Text -> Maybe (Key, Text)
lineToKeyValue line =
  Text.breakOn "=" line
  & (\(rawKey, rawValue) ->
      case Text.stripPrefix "=" rawValue of
        Just value ->
          Just (fromText rawKey, value)
        Nothing ->
          Nothing
    )

