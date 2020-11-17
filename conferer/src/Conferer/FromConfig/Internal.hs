{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Conferer.FromConfig.Internal where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.ByteString (ByteString)
import           Control.Exception
import           Data.Typeable
import           Data.String (IsString, fromString)
import           Text.Read (readMaybe)
import           Data.Dynamic
import           GHC.Generics
import           Data.Function (on, (&))

import           Conferer.Key
import           Conferer.Config.Internal.Types
import Conferer.Config.Internal
import qualified Data.Char as Char
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.List (nub, foldl', sort)

instance {-# OVERLAPPABLE #-} Typeable a => FromConfig a where
  fetchFromConfig key config = do
    fetchFromConfigWith (const Nothing) key config

instance FromConfig () where
  fetchFromConfig _key _config = return ()

instance FromConfig String where
  fetchFromConfig = fetchFromConfigWith (Just . Text.unpack)

instance {-# OVERLAPPABLE #-} (Typeable a, FromConfig a) =>
    FromConfig [a] where
  fetchFromConfig key config = do
    keysForItems <- getSubkeysForItems
    case keysForItems of
      Nothing -> do
        fetchFromDefaults @[a] key config
      Just subkeys -> do
        defaultsMay <- fetchFromDefaults' @[a] key config
        let configWithDefaults :: Config =
              case defaultsMay of
                Just defaults -> 
                  foldl' (\c (index, value) -> 
                    c & addDefault (key /. "defaults" /. fromString (show index)) value) config
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
    getSubkeysForItems ::IO (Maybe [Key])
    getSubkeysForItems = do
      getFromConfig @(Maybe Text) (key /. "keys") config
        >>= \case
          Just rawKeys -> do
            return $
              Just $ 
              nub $ 
              filter (/= "") $ 
              fromString @Key .
              Text.unpack <$>
              Text.split (== ',') rawKeys
          Nothing -> do
            subelements <- 
              sort
              . nub
              . filter (not . (`elem` ["prototype", "keys", "defaults"]))
              . mapMaybe (\k -> case keyPrefixOf key k of
                    Just (Path (subkey:_)) -> Just $ fromText subkey
                    _ -> Nothing)
              <$> listSubkeys key config
            return $ if null subelements then Nothing else Just subelements

instance FromConfig Int where
  fetchFromConfig = fetchFromConfigByRead

instance FromConfig Integer where
  fetchFromConfig k c = do
    fetchFromConfigByRead k c

instance FromConfig Float where
  fetchFromConfig = fetchFromConfigByRead

instance FromConfig ByteString where
  fetchFromConfig = fetchFromConfigWith (Just . Text.encodeUtf8)

instance (Typeable a, FromConfig a) => FromConfig (Maybe a) where
  fetchFromConfig k config = do
    let 
      newConfig =
        case getKeyFromDefaults k config >>= fromDynamic @(Maybe a) of
        Just (Just defaultThing) ->
          config & addDefault k defaultThing
        Just Nothing ->
          config & removeDefault k
        _ ->
          config

    (Just <$> fetchFromConfig @a k newConfig)
      `catch` (\(_e :: MissingRequiredKey) -> return Nothing)

instance FromConfig Text where
  fetchFromConfig = fetchFromConfigWith Just

instance FromConfig Bool where
  fetchFromConfig = fetchFromConfigWith parseBool

parseBool :: Text -> Maybe Bool
parseBool text =
  case Text.toLower text of
    "false" -> Just False
    "true" -> Just True
    _ -> Nothing

fetchFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> IO a
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

fetchFromConfigByIsString :: (Typeable a, IsString a) => Key -> Config -> IO a
fetchFromConfigByIsString = fetchFromConfigWith (Just . fromString . Text.unpack)

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

      FoundInDefaults k dynamic ->
        case fromDynamic dynamic of
          Just a -> do
            return a
          Nothing -> do
            throwTypeMismatchWithDefault @a k dynamic

-- | Main typeclass for defining the way to get values from config, hiding the
-- 'Text' based nature of the 'Source's.
-- updated using a config, so for example a Warp.Settings can get updated from a config,
-- but that doesn't make much sense for something like an 'Int'
--
-- You'd normally would never implement this typeclass, if you want to implement
-- 'FromConfig' you should implement that directly, and if you want to use
-- 'DefaultConfig' and 'FromConfig' to implement 'FromConfig' you should let
-- the default 'Generics' based implementation do it's thing

class DefaultConfig a where
  configDef :: a


class FromConfig a where
  fetchFromConfig :: Key -> Config -> IO a
  default fetchFromConfig :: (Typeable a, Generic a, IntoDefaultsG (Rep a), FromConfigG (Rep a)) => Key -> Config -> IO a
  fetchFromConfig k c = do
    defaultValue <- fetchFromDefaults' @a k c
    let config = 
          case defaultValue of
            Just d -> c & withDefaults' (intoDefaultsG k $ from d)
            Nothing -> c
    to <$> fetchFromConfigG k config

-- | Purely 'Generics' machinery, ignore...
class FromConfigG f where
  fetchFromConfigG :: Key -> Config -> IO (f a)

instance FromConfigG inner =>
    FromConfigG (D1 metadata inner) where
  fetchFromConfigG key config = do
    M1 <$> fetchFromConfigG key config

instance (FromConfigWithConNameG inner, Constructor constructor) =>
    FromConfigG (C1 constructor inner) where
  fetchFromConfigG key config =
    M1 <$> fetchFromConfigWithConNameG @inner (conName @constructor undefined) key config

class FromConfigWithConNameG f where
  fetchFromConfigWithConNameG :: String -> Key -> Config -> IO (f a)

instance (FromConfigWithConNameG left, FromConfigWithConNameG right) =>
    FromConfigWithConNameG (left :*: right) where
  fetchFromConfigWithConNameG s key config = do
    leftValue <- fetchFromConfigWithConNameG @left s key config
    rightValue <- fetchFromConfigWithConNameG @right s key config
    return (leftValue :*: rightValue)

instance (FromConfigG inner, Selector selector) =>
    FromConfigWithConNameG (S1 selector inner) where
  fetchFromConfigWithConNameG s key config =
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
    in M1 <$> fetchFromConfigG @inner (key /. fromText scopedKey) config

-- | Purely 'Generics' machinery, ignore...
instance (FromConfig inner) => FromConfigG (Rec0 inner) where
  fetchFromConfigG key config = do
    K1 <$> fetchFromConfig @inner key config

-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################

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

-- | Purely 'Generics' machinery, ignore...
instance (Typeable inner) => IntoDefaultsG (Rec0 inner) where
  intoDefaultsG key (K1 inner) = do
    [(key, toDyn inner)]

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
    , Text.unpack (keyName key)
    , "' as "
    , show aTypeRep
    ]

throwConfigParsingError :: forall a b. (Typeable a) => Key -> Text -> IO b
throwConfigParsingError key text =
  throwIO $ configParsingError @a  key text

configParsingError :: forall a. (Typeable a) => Key -> Text -> ConfigParsingError
configParsingError key text =
  ConfigParsingError key text $ typeRep (Proxy :: Proxy a)

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

throwMissingRequiredKey :: forall t a. Typeable t => Key -> IO a
throwMissingRequiredKey key =
  throwMissingRequiredKeys @t [key]

missingRequiredKey :: forall t. Typeable t => Key -> MissingRequiredKey
missingRequiredKey key =
  missingRequiredKeys @t [key]

throwMissingRequiredKeys :: forall t a. Typeable t => [Key] -> IO a
throwMissingRequiredKeys keys =
  throwIO $ missingRequiredKeys @t keys

missingRequiredKeys :: forall a. (Typeable a) => [Key] -> MissingRequiredKey
missingRequiredKeys keys =
  MissingRequiredKey keys (typeRep (Proxy :: Proxy a))

data TypeMismatchWithDefault =
  TypeMismatchWithDefault Key Dynamic TypeRep
  deriving (Typeable)

instance Eq TypeMismatchWithDefault where
  (==) = (==) `on` 
    (\(TypeMismatchWithDefault k dyn t) -> (k, show dyn, t))
instance Exception TypeMismatchWithDefault
instance Show TypeMismatchWithDefault where
  show (TypeMismatchWithDefault key dyn aTypeRep) =
    concat
    [ "Couldn't parse the default from key '"
    , Text.unpack (keyName key)
    , "' since there is a type mismatch. "
    , "Expected type is '"
    , show aTypeRep
    , "' but the actual type is '"
    , show dyn
    , "'"
    ]

throwTypeMismatchWithDefault :: forall a b. (Typeable a) => Key -> Dynamic -> IO b
throwTypeMismatchWithDefault key dynamic =
  throwIO $ typeMismatchWithDefault @a  key dynamic

typeMismatchWithDefault :: forall a. (Typeable a) => Key -> Dynamic -> TypeMismatchWithDefault
typeMismatchWithDefault key dynamic =
  TypeMismatchWithDefault key dynamic $ typeRep (Proxy :: Proxy a)

fetchFromDefaults :: forall a. (Typeable a) => Key -> Config -> IO a
fetchFromDefaults key config = 
  fetchFromDefaults' key config >>=
    \case
    Nothing -> do
      throwMissingRequiredKey @a key
    Just a -> 
      return a

fetchFromDefaults' :: forall a. (Typeable a) => Key -> Config -> IO (Maybe a)
fetchFromDefaults' key config =
  case getKeyFromDefaults key config of
    Nothing -> do
      return Nothing
    Just dyn -> 
      case fromDynamic @a dyn of
        Nothing ->
          throwTypeMismatchWithDefault @a key dyn
        Just a -> return $ Just a

-- | Fetch a value from a config under some specific key that's parsed using the 'FromConfig'
--   instance, and as a default it uses the value from 'DefaultConfig'.
--
--   Notes:
--     - This function may throw an exception if parsing fails for any subkey
getFromConfig :: forall a. (FromConfig a) => Key -> Config -> IO a
getFromConfig key config =
  fetchFromConfig key config

-- | Same as 'getFromConfig' using the root key
--
--   Notes:
--     - This function may throw an exception if parsing fails for any subkey
getFromRootConfig :: forall a. (FromConfig a) => Config -> IO a
getFromRootConfig config =
  getFromConfig "" config


-- | Same as 'getFromConfig' but with a user defined default (instead of 'DefaultConfig' instance)
--
--   Useful for fetching primitive types
getFromConfigWithDefault :: forall a. (Typeable a, FromConfig a) => Key -> Config -> a -> IO a
getFromConfigWithDefault key config configDefault =
  getFromConfig key (config & addDefault key configDefault)

getFromRootConfigWithDefault :: forall a. (Typeable a, FromConfig a) => Config -> a -> IO a
getFromRootConfigWithDefault config configDefault =
  getFromRootConfig (config & addDefault "" configDefault)