module Conferer.FetchFromConfig.Basics where
import Conferer.Types
import Conferer (getKey)
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import Debug.Trace

instance FetchFromConfig Int where
    fetch key config = do
        valueAsText <- getKey key config
        return $ case valueAsText of
            Just valueAsText -> case readMaybe (unpack valueAsText) of
                Just value -> Right value
                Nothing -> Left ("Key " <> keyName key <> " could not be parsed correctly")
            Nothing -> Left ("Key " <> keyName key <> " was not found")
