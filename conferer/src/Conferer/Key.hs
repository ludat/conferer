module Conferer.Key
  ( Key
  , (/.)
  , keyPrefixOf
  , isKeyPrefixOf
  , fromText
  , fromString
  , rawKeyComponents
  , unconsKey
  ) where

import Conferer.Key.Internal
import Data.String (IsString(fromString))
