-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Internal module providing FromConfig functionality
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Conferer.FromConfig.Internal where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Exception
import Data.Typeable
import Text.Read (readMaybe)
import Data.Dynamic
import GHC.Generics
import Data.Function (on, (&))
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as Time
import qualified Data.Time.Clock as Time

import Conferer.Key
import Conferer.Config.Internal.Types
import Conferer.Config.Internal
import qualified Data.Char as Char
import Control.Monad (forM)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified System.FilePath as FilePath
import Data.List (nub, foldl', sort)
import Data.String (IsString(..))
import Data.Foldable (msum)

-- | The typeclass for defining the way to get values from a 'Config', hiding the
-- 'Text' based nature of the 'Conferer.Source.Source's and parse whatever value
-- as the types sees fit
--
-- Some of these instances are provided in different packages to avoid the heavy
-- dependencies.
--
-- It provides a reasonable default using 'Generic's so most of the time user need
-- not to implement this typeclass.
class FromConfig a where
  -- | This function uses a 'Config' and a scoping 'Key' to get a value.
  --
  -- Some conventions:
  --
  -- * When some 'Key' is missing this function should throw 'MissingRequiredKey'
  --
  -- * For any t it should hold that @fetchFromConfig k (config & addDefault k t) == t@
  -- meaning that a default on the same key with the right type should be used as a
  -- default and with no configuration that value should be returned
  --
  -- * Try desconstructing the value in as many keys as possible since is allows easier
  -- partial overriding.
  fromConfig :: Key -> Config -> IO a
  default fromConfig :: (Typeable a, Generic a, IntoDefaultsG (Rep a), FromConfigG (Rep a)) => Key -> Config -> IO a
  fromConfig key config = do
    let configWithDefaults = case fetchFromDefaults @a key config of
          Just d -> config & addDefaults (intoDefaultsG key $ from d)
          Nothing -> config
    to <$> fromConfigG key configWithDefaults

fetchFromConfig :: forall a. (FromConfig a, Typeable a) => Key -> Config -> IO a
fetchFromConfig key config =
  case fetchFromDefaults @(OverrideFromConfig a) key config of
    Just (OverrideFromConfig fetch) ->
      fetch key $ removeDefault @(OverrideFromConfig a) key config
    Nothing ->
      fromConfig key config

-- | Utility only typeclass to smooth the naming differences between default values for
-- external library settings
--
-- This typeclass is not used internally it's only here for convinience for users
class DefaultConfig a where
  configDef :: a

instance {-# OVERLAPPABLE #-} Typeable a => FromConfig a where
  fromConfig = fetchRequiredFromDefaults

instance FromConfig () where
  fromConfig _key _config = return ()

instance FromConfig String where
  fromConfig = fetchFromConfigWith (Just . Text.unpack)

instance {-# OVERLAPPABLE #-} (Typeable a, FromConfig a) =>
    FromConfig [a] where
  fromConfig key config = do
    keysForItems <- getSubkeysForItems
    case keysForItems of
      Nothing -> do
        fetchRequiredFromDefaults @[a] key config
      Just subkeys -> do
        let configWithDefaults :: Config =
              case fetchFromDefaults @[a] key config of
                Just defaults ->
                  foldl' (\c (index, value) ->
                    c & addDefault (key /. "defaults" /. mkKey (show index)) value) config
                  $ zip [0 :: Integer ..] defaults
                Nothing -> config
        forM subkeys $ \k -> do
          fetchFromConfig @a (key /. k)
            (if isKeyPrefixOf (key /. "defaults") (key /. k)
              then
                configWithDefaults
              else
                configWithDefaults & addKeyMappings [(key /. k, key /. "prototype")])
    where
    getSubkeysForItems :: IO (Maybe [Key])
    getSubkeysForItems = do
      fetchFromConfig @(Maybe Text) (key /. "keys") config
        >>= \case
          Just rawKeys -> do
            return $
              Just $
              nub $
              filter (/= "") $
              mkKey .
              Text.unpack <$>
              Text.split (== ',') rawKeys
          Nothing -> do
            subelements <-
              sort
              . nub
              . filter (not . (`elem` ["prototype", "keys", "defaults"]))
              . mapMaybe (\k -> case rawKeyComponents <$> stripKeyPrefix key k of
                    Just (subkey:_) -> Just $ fromText subkey
                    _ -> Nothing)
              <$> listSubkeys key config
            return $ if null subelements then Nothing else Just subelements

instance FromConfig Int where
  fromConfig = fetchFromConfigByRead

instance FromConfig Integer where
  fromConfig = fetchFromConfigByRead

instance FromConfig Float where
  fromConfig = fetchFromConfigByRead

instance FromConfig BS.ByteString where
  fromConfig = fetchFromConfigWith (Just . Text.encodeUtf8)

instance FromConfig LBS.ByteString where
  fromConfig = fetchFromConfigWith (Just . LBS.fromStrict . Text.encodeUtf8)

instance forall a. (Typeable a, FromConfig a) => FromConfig (Maybe a) where
  fromConfig key config = do
    let
      configWithUnwrappedDefault =
        case fetchFromDefaults @(Maybe a) key config of
          Just (Just defaultThing) -> do
            config & addDefault key defaultThing
          _ -> do
            config
    (Just <$> fetchFromConfig @a key configWithUnwrappedDefault)
      `catch` (\(_e :: MissingRequiredKey) -> return Nothing)

instance FromConfig Text where
  fromConfig = fetchFromConfigWith Just

instance FromConfig Bool where
  fromConfig = fetchFromConfigWith parseBool

-- | A newtype wrapper for a 'FilePath' to allow implementing 'FromConfig'
-- with something better than just a 'String'
newtype File =
  File FilePath
  deriving (Show, Eq, Ord, Read)

unFile :: File -> FilePath
unFile (File f) = f

instance IsString File where
  fromString s = File s

instance FromConfig File where
  fromConfig key config = do
    let defaultPath = fetchFromDefaults @File key config

    filepath <- fetchFromConfig @(Maybe String) key config

    extension <- fetchFromConfig @(Maybe String) (key /. "extension") config
    dirname <- fetchFromConfig @(Maybe String) (key /. "dirname") config
    basename <- fetchFromConfig @(Maybe String) (key /. "basename") config
    filename <- fetchFromConfig @(Maybe String) (key /. "filename") config

    let
      constructedFilePath =
        applyIfPresent FilePath.replaceDirectory dirname
        $ applyIfPresent FilePath.replaceBaseName basename
        $ applyIfPresent FilePath.replaceExtension extension
        $ applyIfPresent FilePath.replaceFileName filename
        $ fromMaybe (unFile $ fromMaybe "" defaultPath) filepath
    if FilePath.isValid constructedFilePath
      then return $ File constructedFilePath
      else throwMissingRequiredKeys @String
        [ key
        , key /. "extension"
        , key /. "dirname"
        , key /. "basename"
        , key /. "filename"
        ]
    where
      applyIfPresent f maybeComponent =
        (\fp -> maybe fp (f fp) maybeComponent)

instance FromConfig Time.DayOfWeek where
  fromConfig =
    fetchFromConfigWith $
    (\case
      "sunday" -> Just Time.Sunday
      "sun" -> Just Time.Sunday

      "monday" -> Just Time.Monday
      "mon" -> Just Time.Monday

      "tuesday" -> Just Time.Tuesday
      "tue" -> Just Time.Tuesday

      "wednesday" -> Just Time.Wednesday
      "wed" -> Just Time.Wednesday

      "thursday" -> Just Time.Thursday
      "thu" -> Just Time.Thursday

      "friday" -> Just Time.Friday
      "fri" -> Just Time.Friday

      "saturday" -> Just Time.Saturday
      "sat" -> Just Time.Saturday

      _ -> Nothing
      ) . Text.toLower

instance FromConfig Time.Day where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.TimeOfDay where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.UTCTime where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.LocalTime where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.TimeZone where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.ZonedTime where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.CalendarDiffDays where
  fromConfig = fetchFromConfigByIso8601

instance FromConfig Time.CalendarDiffTime where
  fromConfig = fetchFromConfigByIso8601

-- | Helper function to parse a 'Bool' from 'Text'
parseBool :: Text -> Maybe Bool
parseBool text =
  case Text.toLower text of
    "false" -> Just False
    "true" -> Just True
    _ -> Nothing

data OverrideFromConfig a =
  OverrideFromConfig (Key -> Config -> IO a)

-- | Allow the programmer to override this 'FromConfig' instance by providing
-- a special 'OverrideFromConfig' value.
--
-- To avoid infinite recursion we remove the Override before calling the value

-- | Helper function to implement fetchFromConfig using the 'Read' instance
fetchFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> IO a
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

-- | Helper function to implement fetchFromConfig using the 'Time.ISO8601' instance
fetchFromConfigByIso8601 :: (Typeable a, Time.ISO8601 a) => Key -> Config -> IO a
fetchFromConfigByIso8601 = fetchFromConfigWith (Time.iso8601ParseM . Text.unpack)

-- | Helper function to implement fetchFromConfig using the 'IsString' instance
fetchFromConfigByIsString :: (Typeable a, IsString a) => Key -> Config -> IO a
fetchFromConfigByIsString = fetchFromConfigWith (Just . fromString . Text.unpack)

-- | Helper function to implement fetchFromConfig using some parsing function
fetchFromConfigWith :: forall a. Typeable a => (Text -> Maybe a) -> Key -> Config -> IO a
fetchFromConfigWith parseValue key config = do
  getKey key config >>=
    \case
      MissingKey k -> do
        throwMissingRequiredKeys @a k

      FoundInSources k value ->
        case parseValue value of
          Just a -> do
            return a
          Nothing -> do
            throwConfigParsingError @a k value

      FoundInDefaults k dynamics ->
        case fromDynamics dynamics of
          Just a -> do
            return a
          Nothing -> do
            throwMissingRequiredKeys @a [k]

fromDynamics :: forall a. Typeable a => [Dynamic] -> Maybe a
fromDynamics =
  msum . fmap (fromDynamic @a)

-- | Helper function does the plumbing of desconstructing a default into smaller
-- defaults, which is usefull for nested 'fetchFromConfig'.
addDefaultsAfterDeconstructingToDefaults
  :: forall a.
  Typeable a =>
  -- | Function to deconstruct the value
  (a -> [(Key, Dynamic)]) ->
  -- | Key where to look for the value
  Key ->
  -- | The config
  Config ->
  IO Config
addDefaultsAfterDeconstructingToDefaults destructureValue key config = do
  case fetchFromDefaults @a key config of
    Just value -> do
      let newDefaults =
            ((\(k, d) -> (key /. k, d)) <$> destructureValue value)
      return $
        addDefaults newDefaults config
    Nothing -> do
      return config

-- | Helper function to override the fetching function for a certain key.
--
-- This function creates a 'Dynamic' that when added to the defaults allows
-- overriding the default 'FromConfig' instance.
overrideFetch :: forall a. Typeable a => (Key -> Config -> IO a) -> Dynamic
overrideFetch f =
  toDyn @(OverrideFromConfig a) $ OverrideFromConfig f

-- | Exception to show that a value couldn't be parsed properly
data ConfigParsingError =
  ConfigParsingError Key Text TypeRep
  deriving (Typeable, Eq)

instance Exception ConfigParsingError
instance Show ConfigParsingError where
  show (ConfigParsingError key value aTypeRep) =
    concat
    [ "Couldn't parse value '"
    , Text.unpack value
    , "' from key '"
    , show key
    , "' as "
    , show aTypeRep
    ]

-- | Helper function to throw 'ConfigParsingError'
throwConfigParsingError :: forall a b. (Typeable a) => Key -> Text -> IO b
throwConfigParsingError key text =
  throwIO $ configParsingError @a  key text

-- | Helper function to create a 'ConfigParsingError'
configParsingError :: forall a. (Typeable a) => Key -> Text -> ConfigParsingError
configParsingError key text =
  ConfigParsingError key text $ typeRep (Proxy :: Proxy a)

-- | Exception to show that some non optional 'Key' was missing while trying
-- to 'fetchFromConfig'
data MissingRequiredKey =
  MissingRequiredKey [Key] TypeRep
  deriving (Typeable, Eq)

instance Exception MissingRequiredKey
instance Show MissingRequiredKey where
  show (MissingRequiredKey keys aTypeRep) =
    concat
    [ "Failed to get a '"
    , show aTypeRep
    , "' from keys: "
    , Text.unpack
      $ Text.intercalate ", "
      $ fmap (Text.pack . show)
      $ keys

    ]

-- | Simplified helper function to throw a 'MissingRequiredKey'
throwMissingRequiredKey :: forall t a. Typeable t => Key -> IO a
throwMissingRequiredKey key =
  throwMissingRequiredKeys @t [key]

-- | Simplified helper function to create a 'MissingRequiredKey'
missingRequiredKey :: forall t. Typeable t => Key -> MissingRequiredKey
missingRequiredKey key =
  missingRequiredKeys @t [key]

-- | Helper function to throw a 'MissingRequiredKey'
throwMissingRequiredKeys :: forall t a. Typeable t => [Key] -> IO a
throwMissingRequiredKeys keys =
  throwIO $ missingRequiredKeys @t keys

-- | Helper function to create a 'MissingRequiredKey'
missingRequiredKeys :: forall a. (Typeable a) => [Key] -> MissingRequiredKey
missingRequiredKeys keys =
  MissingRequiredKey keys (typeRep (Proxy :: Proxy a))

-- | Fetch from value from the defaults map of a 'Config' or else throw
fetchRequiredFromDefaults :: forall a. (Typeable a) => Key -> Config -> IO a
fetchRequiredFromDefaults key config =
  case fetchFromDefaults key config of
    Nothing -> do
      throwMissingRequiredKey @a key
    Just a ->
      return a

-- | Fetch from value from the defaults map of a 'Config' or else return a 'Nothing'
fetchFromDefaults :: forall a. (Typeable a) => Key -> Config -> Maybe a
fetchFromDefaults key config =
  getKeyFromDefaults key config
    >>= fromDynamics @a

-- | Same as 'fetchFromConfig' using the root key
fetchFromRootConfig :: forall a. (FromConfig a, Typeable a) => Config -> IO a
fetchFromRootConfig =
  fetchFromConfig ""

-- | Same as 'fetchFromConfig' but adding a user defined default before 'fetchFromConfig'ing
-- so it doesn't throw a MissingKeyError
fetchFromConfigWithDefault :: forall a. (Typeable a, FromConfig a) => Config -> Key -> a -> IO a
fetchFromConfigWithDefault config key configDefault =
  fetchFromConfig key (config & addDefault key configDefault)

-- | Same as 'fetchFromConfigWithDefault' using the root key
fetchFromRootConfigWithDefault :: forall a. (Typeable a, FromConfig a) => Config -> a -> IO a
fetchFromRootConfigWithDefault config configDefault =
  fetchFromRootConfig (config & addDefault "" configDefault)

-- | Purely 'Generic's machinery, ignore...
class FromConfigG f where
  fromConfigG :: Key -> Config -> IO (f a)

instance FromConfigG inner =>
    FromConfigG (D1 metadata inner) where
  fromConfigG key config = do
    M1 <$> fromConfigG key config

instance (FromConfigWithConNameG inner, Constructor constructor) =>
    FromConfigG (C1 constructor inner) where
  fromConfigG key config =
    M1 <$> fromConfigWithConNameG @inner (conName @constructor undefined) key config

-- | Purely 'Generic's machinery, ignore...
class FromConfigWithConNameG f where
  fromConfigWithConNameG :: String -> Key -> Config -> IO (f a)

instance (FromConfigWithConNameG left, FromConfigWithConNameG right) =>
    FromConfigWithConNameG (left :*: right) where
  fromConfigWithConNameG s key config = do
    leftValue <- fromConfigWithConNameG @left s key config
    rightValue <- fromConfigWithConNameG @right s key config
    return (leftValue :*: rightValue)

instance (FromConfigG inner, Selector selector) =>
    FromConfigWithConNameG (S1 selector inner) where
  fromConfigWithConNameG s key config =
    let
      applyFirst :: (Char -> Char) -> Text -> Text
      applyFirst f t = case Text.uncons t of
        Just (c, ts) -> Text.cons (f c) ts
        Nothing -> t

      fieldName = Text.pack $ selName @selector undefined
      prefix = applyFirst Char.toLower $ Text.pack s
      scopedKey =
        case Text.stripPrefix prefix fieldName of
          Just stripped -> applyFirst Char.toLower stripped
          Nothing -> fieldName
    in M1 <$> fromConfigG @inner (key /. fromText scopedKey) config

instance (FromConfig inner, Typeable inner) => FromConfigG (Rec0 inner) where
  fromConfigG key config = do
    K1 <$> fetchFromConfig @inner key config

-- | Purely 'Generic's machinery, ignore...
class IntoDefaultsG f where
  intoDefaultsG :: Key -> f a -> [(Key, Dynamic)]

instance IntoDefaultsG inner =>
    IntoDefaultsG (D1 metadata inner) where
  intoDefaultsG key (M1 inner) =
    intoDefaultsG key inner

instance (IntoDefaultsWithConNameG inner, Constructor constructor) =>
    IntoDefaultsG (C1 constructor inner) where
  intoDefaultsG key (M1 inner) =
    intoDefaultsWithConNameG @inner (conName @constructor undefined) key inner

-- | Purely 'Generic's machinery, ignore...
class IntoDefaultsWithConNameG f where
  intoDefaultsWithConNameG :: String -> Key -> f a -> [(Key, Dynamic)]

instance (IntoDefaultsWithConNameG left, IntoDefaultsWithConNameG right) =>
    IntoDefaultsWithConNameG (left :*: right) where
  intoDefaultsWithConNameG s key (left :*: right) = do
    intoDefaultsWithConNameG @left s key left
    ++
    intoDefaultsWithConNameG @right s key right

instance (IntoDefaultsG inner, Selector selector) =>
    IntoDefaultsWithConNameG (S1 selector inner) where
  intoDefaultsWithConNameG s key (M1 inner) =
    let
      applyFirst :: (Char -> Char) -> Text -> Text
      applyFirst f t = case Text.uncons t of
        Just (c, ts) -> Text.cons (f c) ts
        Nothing -> t

      fieldName = Text.pack $ selName @selector undefined
      prefix = applyFirst Char.toLower $ Text.pack s
      scopedKey =
        case Text.stripPrefix prefix fieldName of
          Just stripped -> applyFirst Char.toLower stripped
          Nothing -> fieldName
    in intoDefaultsG @inner (key /. fromText scopedKey) inner

instance (Typeable inner) => IntoDefaultsG (Rec0 inner) where
  intoDefaultsG key (K1 inner) = do
    [(key, toDyn inner)]
