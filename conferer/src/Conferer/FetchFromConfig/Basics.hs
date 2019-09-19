{-# LANGUAGE FlexibleInstances #-}
module Conferer.FetchFromConfig.Basics where

import           Conferer.Types
import           Conferer.Core (getKey, (/.))
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
    Nothing -> Left ("Key " `Text.append` keyName key `Text.append` " could not be parsed correctly")

fetchFromConfigWith :: (Text -> Maybe a) -> Key -> Config -> IO (Either Text a)
fetchFromConfigWith parseValue key config =
  (fromValueWith parseValue key =<<) <$> getKey key config

-- | Concatenate many transformations to the config based on keys and functions
findKeyAndApplyConfig ::
  FetchFromConfig newvalue
  => Config -- ^ Complete config
  -> Key -- ^ Key that indicates the part of the config that we care about
  -> Key -- ^ Key that we use to find the config (usually concatenating with the
         -- other key)
  -> (newvalue -> config -> config) -- ^ Function that knows how to use the
                                    -- value to update the config
  -> Either Text config -- ^ Result of the last config updating
  -> IO (Either Text config) -- ^ Updated config
findKeyAndApplyConfig config k relativeKey f (Right customConfig) =
  fetch (k /. relativeKey) config
    >>= \case
      Left a -> return $ Right customConfig
      Right a -> return $ Right $ f a customConfig
findKeyAndApplyConfig config k relativeKey f (Left e) = return $ Left e
