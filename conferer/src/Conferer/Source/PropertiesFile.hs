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

-- | 'SourceCreator' for properties file 'Source' that read from a
-- config file in @config/{env}.properties@ and parses it as a properties
-- file with @some.key=a value@ lines
fromConfig :: Key -> SourceCreator
fromConfig key config = do
  filePath <- getFilePathFromEnv key "properties" config
  fromFilePath filePath

fromFilePath :: FilePath -> IO Source
fromFilePath filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      fileContent <- Text.readFile filePath
      return $ fromFileContent fileContent
    else
      return Null.empty

-- | 'SourceCreator' for properties file 'Source' that only parses a
-- given 'Text' as a properties file
fromFileContent :: Text -> Source
fromFileContent fileContent = do
  let keyValues =
        fileContent
        & Text.lines
        & fmap lineToKeyValue
        & catMaybes
  InMemory.fromAssociations keyValues

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

