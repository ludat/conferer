{-# LANGUAGE FlexibleInstances #-}
module Conferer.FetchFromConfig.Basics where

import           Conferer.Types
import           Conferer.Core (getKey)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.ByteString (ByteString)

import           Data.String (IsString, fromString)

import           Text.Read (readMaybe)

instance FetchFromConfig Int where
  fetch = fetchFromConfigByRead

instance FetchFromConfig Float where
  fetch = fetchFromConfigByRead

fetchFromConfigByRead :: Read a => Key -> Config -> IO (Either Text a)
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

instance FetchFromConfig ByteString where
  fetch = fetchFromConfigWith (Just . Text.encodeUtf8)

instance FetchFromConfig String where
  fetch = fetchFromConfigWith (Just . Text.unpack)

instance FetchFromConfig Text where
  fetch = fetchFromConfigWith (Just)

instance FetchFromConfig Bool where
    fetch = fetchFromConfigWith parseBool
        where
          parseBool text =
            case Text.toLower text of
              "false" -> Just False
              "true" -> Just True
              _ -> Nothing

fromValueWith :: (Text -> Maybe a) -> Key -> Text -> Either Text a
fromValueWith parseValue key valueAsText = case parseValue valueAsText of
    Just value -> Right value
    Nothing -> Left ("Key " <> keyName key <> " could not be parsed correctly")

fetchFromConfigWith :: (Text -> Maybe a) -> Key -> Config -> IO (Either Text a)
fetchFromConfigWith parseValue key config =
  (fromValueWith parseValue key =<<) <$> getKey key config
