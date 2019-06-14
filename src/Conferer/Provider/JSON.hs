module Conferer.Provider.JSON where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad (join)
import           Data.Vector
import           Text.Read (readMaybe)

import Conferer.Types


boolToString :: Bool -> Text
boolToString True = "true"
boolToString False = "false"

valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText (Object o) = Nothing
valueToText (Array as) = Nothing
valueToText (Number n) = Just $ Text.pack $ show n
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

mkJsonConfigProvider :: Value -> ConfigProvider
mkJsonConfigProvider v =
  ConfigProvider
  { getKeyInProvider = \k -> do
      return $ traverseJSON k v
  }
