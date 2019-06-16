module Conferer.Provider.JSON where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad (join)
import           Data.Vector
import           Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Directory (doesFileExist)

import Conferer.Provider.Files
import Conferer.Provider.Null
import Conferer.Types


boolToString :: Bool -> Text
boolToString True = "true"
boolToString False = "false"

valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText (Object o) = Nothing
valueToText (Array as) = Nothing
valueToText (Number n) = Just $ T.decodeUtf8 $ B.toStrict $ encode $ Number n
valueToText (Bool b) = Just $ boolToString b
valueToText (Null) = Nothing

traverseJSON :: Key -> Value -> Maybe Text
traverseJSON (Path []) v = valueToText v
traverseJSON (Path (k:ks)) (Object o) =
  HashMap.lookup k o >>= traverseJSON (Path ks)
traverseJSON (Path (k:ks)) (Array vs) = do
  n :: Int <- readMaybe $ Text.unpack k
  value <- vs !? n
  valueToText value
traverseJSON (Path (k:ks)) _ = Nothing

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Error _) = Nothing
resultToMaybe (Success a) = Just a


mkJsonConfigProvider :: ProviderCreator
mkJsonConfigProvider config = do
  fileToParse <- getFilePathFromEnv config "json"
  fileExists <- doesFileExist fileToParse
  if fileExists
    then do
      value <- decodeFileStrict' fileToParse
      case value of
        Nothing ->
          error $ "Failed to decode file '" <> fileToParse <> "'"
        Just v -> do
          mkJsonConfigProvider' v config
    else do
      mkNullProvider config

mkJsonConfigProvider' :: Value -> ProviderCreator
mkJsonConfigProvider' v = \config ->
  return $ ConfigProvider
  { getKeyInProvider = \k -> do
      return $ traverseJSON k v
  }
