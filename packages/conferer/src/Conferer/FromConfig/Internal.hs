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
{-# LANGUAGE CPP #-}

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
import Data.Function ((&))

import Conferer.Key
import Conferer.Config.Internal.Types
import Conferer.FromConfig.Internal.Types
import Conferer.Config.Internal
import qualified Data.Char as Char
import Control.Monad (forM)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified System.FilePath as FilePath
import Data.List (nub, foldl', sort)
import Data.String (IsString(..))
#if __GLASGOW_HASKELL__ < 808
import Data.Void (absurd)
#endif

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
    let configWithDefaults = case getKeyFromDefaults @a key config of
          FoundInDefaults d _ -> config & addDefaults (intoDefaultsG key $ from d)
          _ -> config
    to <$> fromConfigG key configWithDefaults

fetchFromConfig :: forall a. (FromConfig a, Typeable a) => Key -> Config -> IO a
fetchFromConfig key config =
  case getKeyFromDefaults @(OverrideFromConfig a) key config of
    FoundInDefaults (OverrideFromConfig fetch) _ ->
      fetch key $ removeDefault @(OverrideFromConfig a) key config
    _ ->
      fromConfig key config

-- | Utility only typeclass to smooth the naming differences between default values for
-- external library settings
--
-- This typeclass is not used internally it's only here for convinience for users
class DefaultConfig a where
  configDef :: a

instance {-# OVERLAPPABLE #-} Typeable a => FromConfig a where
  fromConfig key config =
    case getKeyFromDefaults key config of
      FoundInDefaults a _key ->
        pure a
      MissingKey () keys ->
        throwMissingRequiredKeys @a keys config
#if __GLASGOW_HASKELL__ < 808
      FoundInSources v _ _ -> absurd v
#endif


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
        case getKeyFromDefaults @[a] key config of
          FoundInDefaults a _keys ->
            pure a
          MissingKey () keys ->
            throwMissingRequiredKeys @String (fmap (/. "keys") keys) config
#if __GLASGOW_HASKELL__ < 808
          (FoundInSources v _ _) ->
            absurd v
#endif
      Just subkeys -> do
        let configWithDefaults :: Config =
              case getKeyFromDefaults @[a] key config of
                FoundInDefaults defaults _ ->
                  foldl' (\c (index, value) ->
                    c & addDefault (key /. "defaults" /. mkKey (show index)) value) config
                  $ zip [0 :: Integer ..] defaults
                _ -> config
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
        case getKeyFromDefaults @(Maybe a) key config of
          FoundInDefaults (Just defaultThing) _ -> do
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
    let defaultPath =
          case getKeyFromDefaults @File key config of
            FoundInDefaults v _ -> Just v
            _ -> Nothing

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
      else throwMissingRequiredKey @File key config
    where
      applyIfPresent f maybeComponent =
        (\fp -> maybe fp (f fp) maybeComponent)

-- | Helper function to parse a 'Bool' from 'Text'
parseBool :: Text -> Maybe Bool
parseBool text =
  case Text.toLower text of
    "false" -> pure False
    "f" -> pure False
    "no" -> pure False
    "n" -> pure False
    "0" -> pure False

    "true" -> pure True
    "t" -> pure True
    "yes" -> pure True
    "y" -> pure True
    "1" -> pure False
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

-- | Helper function to implement fetchFromConfig using the 'IsString' instance
fetchFromConfigByIsString :: (Typeable a, IsString a) => Key -> Config -> IO a
fetchFromConfigByIsString = fetchFromConfigWith (Just . fromString . Text.unpack)

-- | Helper function to implement fetchFromConfig using some parsing function
fetchFromConfigWith :: forall a. Typeable a => (Text -> Maybe a) -> Key -> Config -> IO a
fetchFromConfigWith parseValue key config = do
  getKey @a key config >>=
    \case
      MissingKey () k -> do
        throwMissingRequiredKeys @a k config

      FoundInSources value index k ->
        case parseValue value of
          Just a -> do
            return a
          Nothing -> do
            throwConfigParsingError @a k value index config

      FoundInDefaults a _k ->
        return a

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
  case getKeyFromDefaults @a key config of
    FoundInDefaults value _ -> do
      let newDefaults =
            ((\(k, d) -> (key /. k, d)) <$> destructureValue value)
      return $
        addDefaults newDefaults config
    _ -> do
      return config

-- | Helper function to override the fetching function for a certain key.
--
-- This function creates a 'Dynamic' that when added to the defaults allows
-- overriding the default 'FromConfig' instance.
overrideFetch :: forall a. Typeable a => (Key -> Config -> IO a) -> Dynamic
overrideFetch f =
  toDyn @(OverrideFromConfig a) $ OverrideFromConfig f

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
