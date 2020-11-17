module Conferer.Source.PropertiesFile where

import           Data.Text (Text)
import           Data.Function ((&))
import           System.Directory (doesFileExist)
import           Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Conferer.Source
import           Conferer.Source.Files
import           Conferer.Source.Null
import           Conferer.Source.Simple

-- | 'SourceCreator' for properties file 'Source' that read from a
-- config file in @config/{env}.properties@ and parses it as a properties
-- file with @some.key=a value@ lines
mkPropertiesFileSource :: SourceCreator
mkPropertiesFileSource = \config -> do
  filePath <- getFilePathFromEnv config "properties"
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      fileContent <- Text.readFile filePath
      mkPropertiesFileSource' fileContent config
    else do
      mkNullSource config


-- | 'SourceCreator' for properties file 'Source' that only parses a
-- given 'Text' as a properties file
mkPropertiesFileSource' :: Text -> SourceCreator
mkPropertiesFileSource' fileContent =
  \config -> do
    let keyValues =
          fileContent
          & Text.lines
          & fmap lineToKeyValue
          & catMaybes
    mkMapSource keyValues config


-- | Transform a line into a key/value pair (or not)
lineToKeyValue :: Text -> Maybe (Key, Text)
lineToKeyValue line =
  Text.breakOn "=" line
  & (\(rawKey, rawValue) ->
      case Text.stripPrefix "=" rawValue of
        Just value ->
          Just (Path $ Text.splitOn "." rawKey, value)
        Nothing ->
          Nothing
    )

