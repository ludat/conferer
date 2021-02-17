-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Internal module for Key related features
module Conferer.Key.Internal where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function (on)
import Data.List (stripPrefix, isPrefixOf)
import qualified Data.Char as Char
-- | This type is used extensivelly as a way to point into a 'Conferer.Source.Source'
--   and in turn into a 'Conferer.Config.Config'. The intended way to create them is
--   is using 'mkKey'.
--
--   It's a list of alphanumeric words and each 'Conferer.Source.Source' can interpret
--   it as it sees fit.
newtype Key = Key
  { unKey :: [Text]
  } deriving (Eq, Ord)

instance Show Key where
  show key = "\"" ++ Text.unpack (keyName key) ++ "\""

instance IsString Key where
  fromString = mkKey

-- | Helper function to create 'Key's, this function always works, but
--   since 'Key's reject some string this function transforms the input
--   to provide lawful 'Key's instead of throwing.
--
--   For example:
--
--   > 'mkKey' "sOmE.KEy" == "some.key"
--   > 'mkKey' "1.key" == "1.key"
--   > 'mkKey' "1_thing.key" == "1thing.key"
--   > 'mkKey' "some....key" == "some.key"
--   > 'mkKey' ".." == ""
mkKey :: String -> Key
mkKey s =
  Key .
  filter (/= mempty) .
  fmap (Text.filter isKeyCharacter . Text.toLower) .
  Text.split (== '.') .
  fromString
  $ s

-- | Same as 'mkKey' but for 'Text'
fromText :: Text -> Key
fromText = mkKey . Text.unpack

-- | Collapse a key into a textual representation
keyName :: Key -> Text
keyName = Text.intercalate "." . unKey

-- | This function tells if a key is a subkey of another key based
--   using key fragments instead of letters as units
--
-- > 'isKeyPrefixOf' "foo" "foo.bar" == True
-- > 'isKeyPrefixOf' "foo" "foo" == True
-- > 'isKeyPrefixOf' "foo" "fooa" == False
isKeyPrefixOf :: Key -> Key -> Bool
isKeyPrefixOf = isPrefixOf `on` unKey

-- | Given k1 and k2 this function drops k1 as a prefix from k2, if
--   k1 is not a prefix of k2 it returns 'Nothing'
--
-- > 'keyPrefixOf' "foo" "foo.bar" == Just "bar"
-- > 'keyPrefixOf' "foo" "foo" == Just ""
-- > 'keyPrefixOf' "foo" "fooa" == Nothing
-- > 'keyPrefixOf' "" k == Just k
stripKeyPrefix :: Key -> Key -> Maybe Key
stripKeyPrefix key1 key2 = Key <$> (stripPrefix `on` unKey) key1 key2

-- | Concatenate two keys
(/.) :: Key -> Key -> Key
parent /. child = Key (unKey parent ++ unKey child)

-- | Get raw components from a key, usually to do some manipulation
rawKeyComponents :: Key -> [Text]
rawKeyComponents = unKey

-- | Get first component of a key and the rest of the key
unconsKey :: Key -> Maybe (Text, Key)
unconsKey (Key []) = Nothing
unconsKey (Key (k:ks)) = Just (k, Key ks)

-- | Check if a 'Text' is a valid fragment of a 'Key', meaning
-- that each character 'isKeyCharacter'
isValidKeyFragment :: Text -> Bool
isValidKeyFragment t =
  Text.all isKeyCharacter t

-- | Checks if the given 'Char' is a valid for a 'Key'.
-- Meaning it is a lower case ascii letter or a number.
isKeyCharacter :: Char -> Bool
isKeyCharacter c =
  Char.isAsciiLower c || Char.isDigit c