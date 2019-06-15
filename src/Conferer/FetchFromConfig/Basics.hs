module Conferer.FetchFromConfig.Basics where
import Conferer.Types
import Conferer (getKey)
import Data.Text (Text(..), pack, unpack, toLower)
import Text.Read (readMaybe)
import Debug.Trace

instance FetchFromConfig Int where
    fetch = fetchFromConfigWith parseInt
        where parseInt = readMaybe . unpack

instance FetchFromConfig Bool where
    fetch = fetchFromConfigWith parseBool
        where parseBool text = case toLower text of
                                    "false" -> Just False
                                    "true" -> Just True
                                    _ -> Nothing

fromValueWith :: (Text -> Maybe a) -> Key -> Text -> Either Text a
fromValueWith parseValue key valueAsText = case parseValue valueAsText of
    Just value -> Right value
    Nothing -> Left ("Key " <> keyName key <> " could not be parsed correctly")

fetchFromConfigWith :: (Text -> Maybe a) -> Key -> Config -> IO (Either Text a)
fetchFromConfigWith parseValue key config = (fromValueWith parseValue key =<<) <$> getKey key config
