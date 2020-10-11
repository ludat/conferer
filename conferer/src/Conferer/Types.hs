{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Conferer.Types where

import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import           Control.Exception
import           Data.Typeable
import           GHC.Generics
import Data.Function (on)
import Data.List (isPrefixOf)

-- | Core interface for library provided configuration, basically consists of
--   getting a 'Key' and informing returning a maybe signaling the value and
--   if it's present in that specific source

data Source = forall s. (IsSource s, Show s) => Source s

instance Show Source where
  show (Source s) = "Source " ++ show s

class IsSource s where
  getKeyInSource :: s ->  Key -> IO (Maybe Text)
  getSubkeysInSource :: s -> Key -> IO [Key]

instance IsSource Source where
  getKeyInSource (Source source) =
    getKeyInSource source
  getSubkeysInSource (Source source) =
    getSubkeysInSource source


-- | The way to index 'Source's, basically list of names that will be adapted
--   to whatever the source needs
newtype Key
  = Path { unKey :: [Text] }
  deriving (Show, Eq, Ord)

instance IsString Key where
  fromString s =
    Path .
    fmap (Text.toLower) .
    filter (/= mempty) .
    Text.split (== '.') .
    fromString
    $ s

-- | Collapse a key into a textual representation
keyName :: Key -> Text
keyName = Text.intercalate "." . unKey

isKeyPrefixOf :: Key -> Key -> Bool
isKeyPrefixOf = isPrefixOf `on` unKey

-- | Core type that the user of this library interact with, in the future it may
--   contain more this besides a list of sources
data Config =
  Config
  { sources :: [Source]
  , defaults :: Map Key Text
  }

-- | The type for creating a source given a 'Config', some sources require a
-- certain configuration to be initialized (for example: the redis source
-- needs connection info to connect to the server)
type SourceCreator = Config -> IO Source

keyNotPresentError :: forall a. (Typeable a) => Key -> Proxy a -> FailedToFetchError
keyNotPresentError key =
  throw $ FailedToFetchError key $ typeRep (Proxy :: Proxy a)

-- | Default defining instance
--
-- Here a 'Nothing' means that the value didn't appear in the config, some
-- instances never return a value since they have defaults that can never
-- fail
class DefaultConfig a where
  configDef :: a

-- | Main typeclass for defining the way to get values from config, hiding the
-- 'Text' based nature of the 'Source's.
-- updated using a config, so for example a Warp.Settings can get updated from a config,
-- but that doesn't make much sense for something like an 'Int'
--
-- You'd normally would never implement this typeclass, if you want to implement
-- 'FromConfig' you should implement that directly, and if you want to use
-- 'DefaultConfig' and 'FromConfig' to implement 'FromConfig' you should let
-- the default 'Generics' based implementation do it's thing
class FromConfig a where
  updateFromConfig :: Key -> Config -> a -> IO a
  default updateFromConfig :: (Generic a, FromConfigG (Rep a)) => Key -> Config -> a -> IO a
  updateFromConfig k c a = to <$> updateFromConfigG k c (from a)

  fetchFromConfig :: Key -> Config -> IO (Maybe a)
  default fetchFromConfig :: (Generic a, FromConfigG (Rep a)) => Key -> Config -> IO (Maybe a)
  fetchFromConfig k c = fmap to <$> fetchFromConfigG k c

-- | Purely 'Generics' machinery, ignore...
class FromConfigG f where
  updateFromConfigG :: Key -> Config -> f a -> IO (f a)
  fetchFromConfigG :: Key -> Config -> IO (Maybe (f a))

data ConfigParsingError =
  ConfigParsingError Key Text TypeRep
  deriving (Typeable, Eq)

instance Show ConfigParsingError where
  show (ConfigParsingError key value aTypeRep) =
    concat
    [ "Couldn't parse value '"
    , Text.unpack value
    , "' from key '"
    , Text.unpack (keyName key)
    , "' as "
    , show aTypeRep
    ]

instance Exception ConfigParsingError

data FailedToFetchError =
  FailedToFetchError Key TypeRep
  deriving (Typeable, Eq)

instance Show FailedToFetchError where
  show (FailedToFetchError key aTypeRep) =
    concat
    [ "Couldn't get a "
    , show aTypeRep
    , " from key '"
    , Text.unpack (keyName key)
    , "'"
    ]

instance Exception FailedToFetchError

data MissingRequiredKey =
  MissingRequiredKey Key TypeRep
  deriving (Typeable, Eq)

instance Show MissingRequiredKey where
  show (MissingRequiredKey key aTypeRep) =
    concat
    [ "Couldn't get a required "
    , show aTypeRep
    , " from key '"
    , Text.unpack (keyName key)
    , "'"
    ]

instance Exception MissingRequiredKey
