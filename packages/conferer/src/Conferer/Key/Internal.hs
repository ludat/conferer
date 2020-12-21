module Conferer.Key.Internal where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function (on)
import Data.List (stripPrefix, isPrefixOf)
import qualified Data.Char as Char
-- | Core interface for library provided configuration, basically consists of
--   getting a 'Key' and informing returning a maybe signaling the value and
--   if it's present in that specific source

-- | The way to index 'Source's, basically list of names that will be adapted
--   to whatever the source needs
newtype Key = Path 
  { unKey :: [Text]
  } deriving (Eq, Ord)

instance Show Key where
  show key = "\"" ++ Text.unpack (keyName key) ++ "\""

instance IsString Key where
  fromString s =
    Path .
    fmap (Text.filter Char.isAlphaNum . Text.toLower) .
    filter (/= mempty) .
    Text.split (== '.') .
    fromString
    $ s

fromText :: Text -> Key
fromText = fromString . Text.unpack

-- | Collapse a key into a textual representation
keyName :: Key -> Text
keyName = Text.intercalate "." . unKey

-- >>> isKeyPrefixOf "foo" "foo.bar"
-- True
isKeyPrefixOf :: Key -> Key -> Bool
isKeyPrefixOf = isPrefixOf `on` unKey

-- >>> keyPrefixOf "foo" "foo.bar"
-- Just "bar"
keyPrefixOf :: Key -> Key -> Maybe Key
keyPrefixOf key1 key2 = Path <$> (stripPrefix `on` unKey) key1 key2

-- | Create a new 'Key' by concatenating two existing keys.
(/.) :: Key -> Key -> Key
parent /. child = Path (unKey parent ++ unKey child)

rawKeyComponents :: Key -> [Text]
rawKeyComponents = unKey

unconsKey :: Key -> Maybe (Text, Key)
unconsKey (Path []) = Nothing
unconsKey (Path (k:ks)) = Just (k, Path ks)