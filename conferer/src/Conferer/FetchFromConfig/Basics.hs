{-# LANGUAGE FlexibleInstances, FlexibleContexts, DerivingVia, StandaloneDeriving #-}

module Conferer.FetchFromConfig.Basics where
import Conferer.Types
import Conferer.Core (getKey)
import Data.Text (Text, unpack, toLower)
import Data.ByteString (ByteString)
import Data.String (IsString, fromString)
import Text.Read (readMaybe)

deriving via FetchAsRead Int instance FetchFromConfig Int
deriving via FetchAsRead Float instance FetchFromConfig Float

newtype FetchAsRead a = FetchAsRead { getFetchAsRead :: a }

instance (Read a) => FetchFromConfig (FetchAsRead a) where
    fetch = fetchFromConfigWith parseReadable
        where parseReadable text = FetchAsRead <$> (readMaybe $ unpack $ text)

instance FetchFromConfig Bool where
    fetch = fetchFromConfigWith parseBool
        where parseBool text = case toLower text of
                                    "false" -> Just False
                                    "true" -> Just True
                                    _ -> Nothing

newtype FromString a = FromString { getFromString :: a }

instance (IsString a) => FetchFromConfig (FromString a) where
    fetch = fetchFromConfigWith (pure . FromString . fromString . unpack)

deriving via FromString [Char] instance FetchFromConfig [Char]
deriving via FromString ByteString instance FetchFromConfig ByteString
deriving via FromString Text instance FetchFromConfig Text

fromValueWith :: (Text -> Maybe a) -> Key -> Text -> Either Text a
fromValueWith parseValue key valueAsText = case parseValue valueAsText of
    Just value -> Right value
    Nothing -> Left ("Key " <> keyName key <> " could not be parsed correctly")

fetchFromConfigWith :: (Text -> Maybe a) -> Key -> Config -> IO (Either Text a)
fetchFromConfigWith parseValue key config =
  (fromValueWith parseValue key =<<) <$> getKey key config
