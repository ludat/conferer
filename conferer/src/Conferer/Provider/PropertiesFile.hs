module Conferer.Provider.PropertiesFile where

import           Data.Text (Text)
import           Data.Function ((&))
import           System.Directory (doesFileExist)
import           Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Conferer.Types
import           Conferer.Provider.Files
import           Conferer.Provider.Null
import           Conferer.Provider.Simple

-- | 'ProviderCreator' for properties file 'Provider' that read from a
-- config file in @config/{env}.properties@ and parses it as a properties
-- file with @some.key=a value@ lines
mkPropertiesFileProvider :: ProviderCreator
mkPropertiesFileProvider = \config -> do
  filePath <- getFilePathFromEnv config "properties"
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      fileContent <- Text.readFile filePath
      mkPropertiesFileProvider' fileContent config
    else do
      mkNullProvider config


-- | 'ProviderCreator' for properties file 'Provider' that only parses a
-- given 'Text' as a properties file
mkPropertiesFileProvider' :: Text -> ProviderCreator
mkPropertiesFileProvider' fileContent =
  \config -> do
    let keyValues =
          fileContent
          & Text.lines
          & fmap lineToKeyValue
          & catMaybes
    mkMapProvider keyValues config


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

