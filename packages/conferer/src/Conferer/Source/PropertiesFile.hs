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
import System.Directory
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Conferer.Source
import Conferer.Source.Files
import qualified Conferer.Source.Null as Null
import qualified Conferer.Source.InMemory as InMemory
import Data.List

-- | 'Source' that uses a config file in @config/{env}.properties@ and
-- parses it as a properties file with @some.key=a value@ lines
data PropertiesFileSource
  = PropertiesFileSource
    { originalFilePath :: FilePath
    , rawMap :: InMemory.RawInMemorySource
    }
  deriving (Show)

instance IsSource PropertiesFileSource where
  getKeyInSource PropertiesFileSource{..} key = do
    return $ InMemory.lookupKey key rawMap

  getSubkeysInSource PropertiesFileSource{..} key = do
    return $ InMemory.subKeys key rawMap

  explainNotFound PropertiesFileSource{..} key =
    concat
    [ "Adding a new line '"
    , concat $ intersperse "." $ fmap Text.unpack $ rawKeyComponents key
    , "=some value' to the file '"
    , originalFilePath
    , "'"
    ]
  explainSettedKey PropertiesFileSource{..} key =
    concat
    [ "key '"
    , concat $ intersperse "." $ fmap Text.unpack $ rawKeyComponents key
    , "' (on file '"
    , originalFilePath
    , "')"
    ]


-- | Create a 'SourceCreator' using 'getFilePathFromEnv' to get the path to file
-- and 'fromFilePath'
fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "properties" config
  fromFilePath' filePath

-- | Create a 'SourceCreator' reading the file and using that as a properties file, but
-- if the file doesn't exist do nothing.
fromFilePath :: FilePath -> SourceCreator
fromFilePath filepath _config =
  fromFilePath' filepath

-- | Create a 'Source' reading the file and using that as a properties file, but
-- if the file doesn't exist do nothing.
fromFilePath' :: FilePath -> IO Source
fromFilePath' relativeFilePath = do
  filePath <- makeAbsolute relativeFilePath
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      fileContent <- Text.readFile filePath
      return $ fromFileContent filePath fileContent
    else
      return $ fromNonExistentFilepath filePath

-- | Create a 'Source' using some content as a properties file
fromFileContent :: FilePath -> Text -> Source
fromFileContent originalFilePath fileContent =
  let keyValues =
        fileContent
        & Text.lines
        & fmap lineToKeyValue
        & catMaybes
      rawMap = InMemory.rawFromAssociations keyValues
  in Source $ PropertiesFileSource {..}

fromNonExistentFilepath :: FilePath -> Source
fromNonExistentFilepath filePath =
  Source $ Null.NullSource
    { nullExplainNotFound = \key ->
      concat
      [ "Creating a file '"
      , filePath
      , "' (it doesn't exist now) and adding a line '"
      , concat $ intersperse "." $ fmap Text.unpack $ rawKeyComponents key
      , "=some value'."
      ]
    }

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

