{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FetchFromConfig.Basics where

import           Control.Monad (join)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Control.Exception
import           Data.Char (toLower)
import           Data.Typeable (Typeable, typeRep, Proxy(..))
import           Data.String (IsString, fromString)
import           Text.Read (readMaybe)
import           GHC.Generics

import           Conferer.Types
import           Conferer.Core (getKey, (/.), getFromConfig)

fetch :: forall a. (Typeable a, UpdateFromConfig a, DefaultConfig a) => Key -> Config -> IO (Maybe a)
fetch key config = do
  resultadoCompleto <- evaluate =<< fetchFromConfig key config
  case resultadoCompleto of
    Just resultado -> return $ Just resultado
    Nothing -> do
      result :: Either FailedToFetchError a <- try . (evaluate =<<) . getFromConfig key $ config
      case result of
        Right a -> return . Just $ a
        Left e -> return Nothing

instance DefaultConfig Int
instance UpdateFromConfig Int where
  updateFromConfig = updateFromConfigByRead
  fetchFromConfig = fetchFromConfigByRead

instance DefaultConfig Integer
instance UpdateFromConfig Integer where
  updateFromConfig = updateFromConfigByRead
  fetchFromConfig = fetchFromConfigByRead

instance DefaultConfig Float
instance UpdateFromConfig Float where
  updateFromConfig = updateFromConfigByRead
  fetchFromConfig = fetchFromConfigByRead

instance DefaultConfig ByteString
instance UpdateFromConfig ByteString where
  updateFromConfig = updateFromConfigWith (Just . Text.encodeUtf8)
  fetchFromConfig = fetchFromConfigWith (Just . Text.encodeUtf8)

instance DefaultConfig (Maybe a) where
  configDef = Nothing
instance (UpdateFromConfig a) => UpdateFromConfig (Maybe a) where
  updateFromConfig k config (Just a) =
    Just <$> updateFromConfig k config a
  updateFromConfig k config Nothing =
    fetchFromConfig k config
  fetchFromConfig k config = do
    fmap Just <$> fetchFromConfig @a k config


instance DefaultConfig String
instance UpdateFromConfig String where
  updateFromConfig = updateFromConfigWith (Just . Text.unpack)
  fetchFromConfig = fetchFromConfigWith (Just . Text.unpack)

instance DefaultConfig Text
instance UpdateFromConfig Text where
  updateFromConfig = updateFromConfigWith Just
  fetchFromConfig = fetchFromConfigWith Just

parseBool text =
  case Text.toLower text of
    "false" -> Just False
    "true" -> Just True
    _ -> Nothing
instance DefaultConfig Bool
instance UpdateFromConfig Bool where
  updateFromConfig = updateFromConfigWith parseBool
  fetchFromConfig = fetchFromConfigWith parseBool

updateFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> a -> IO (a)
updateFromConfigByRead = updateFromConfigWith (readMaybe . Text.unpack)

fetchFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> IO (Maybe a)
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

fromValueWith :: (Text -> Maybe a) -> Text -> Maybe a
fromValueWith parseValue valueAsText = parseValue valueAsText

fetchFromConfigWith :: forall a. Typeable a => (Text -> Maybe a) -> Key -> Config -> IO (Maybe a)
fetchFromConfigWith parseValue key config = do
  getKey key config >>=
    \case
      Just value ->
        return $ fromValueWith parseValue value
      Nothing ->
        return Nothing

updateFromConfigWith :: forall a. Typeable a => (Text -> Maybe a) -> Key -> Config -> a -> IO a
updateFromConfigWith parseValue key config a = do
  getKey key config >>=
    \case
      Just value ->
        return $
          fromMaybe (throw $ ConfigParsingError key value (typeRep (Proxy :: Proxy a))) $
          fromValueWith parseValue value
      Nothing -> return a

-- | Concatenate many transformations to the config based on keys and functions
findKeyAndApplyConfig ::
  forall newvalue config.
  UpdateFromConfig newvalue
  => Config -- ^ Complete config
  -> Key -- ^ Key that indicates the part of the config that we care about
  -> Key -- ^ Key that we use to find the config (usually concatenating with the
         -- other key)
  -> (config -> newvalue) -- ^ Function that knows how to use the
                                    -- value to update the config
  -> (newvalue -> config -> config) -- ^ Function that knows how to use the
                                    -- value to update the config
  -> config -- ^ Result of the last config updating
  -> IO config -- ^ Updated config
findKeyAndApplyConfig config k relativeKey get set customConfig = do
  newValue <- updateFromConfig @newvalue (k /. relativeKey) config (get customConfig) 
  return $ set newValue customConfig

instance UpdateFromConfigG inner =>
    UpdateFromConfigG (D1 metadata inner) where
  updateFromConfigG key config (M1 inner) =
    M1 <$> updateFromConfigG key config inner
  fetchFromConfigG key config =
    fmap M1 <$> fetchFromConfigG key config

instance (UpdateFromConfigWithConNameG inner, Constructor constructor) =>
    UpdateFromConfigG (C1 constructor inner) where
  updateFromConfigG key config (M1 inner) =
    M1 <$> updateFromConfigWithConNameG @inner (conName @constructor undefined) key config inner
  fetchFromConfigG key config =
    fmap M1 <$> fetchFromConfigWithConNameG @inner (conName @constructor undefined) key config

class UpdateFromConfigWithConNameG f where
  updateFromConfigWithConNameG :: String -> Key -> Config -> f a -> IO (f a)
  fetchFromConfigWithConNameG :: String -> Key -> Config -> IO (Maybe (f a))

instance (UpdateFromConfigWithConNameG left, UpdateFromConfigWithConNameG right) =>
    UpdateFromConfigWithConNameG (left :*: right) where
  updateFromConfigWithConNameG s key config (left :*: right) = do
    leftValue <- updateFromConfigWithConNameG @left s key config left
    rightValue <- updateFromConfigWithConNameG @right s key config right
    return (leftValue :*: rightValue)

  fetchFromConfigWithConNameG s key config = do
    leftValue <- fetchFromConfigWithConNameG @left s key config
    rightValue <- fetchFromConfigWithConNameG @right s key config
    case (leftValue, rightValue) of
      (Just l, Just r) -> return $ Just (l :*: r)
      _ -> return Nothing

instance (UpdateFromConfigG inner, Selector selector) =>
    UpdateFromConfigWithConNameG (S1 selector inner) where
  updateFromConfigWithConNameG s key config (M1 inner) =
    let
      applyFirst :: (Char -> Char) -> Text -> Text
      applyFirst f t = case Text.uncons t of
        Just (c, ts) -> Text.cons (f c) ts
        Nothing -> t

      fieldName = Text.pack $ selName @selector undefined
      prefix = applyFirst toLower $ Text.pack  s
      scopedKey =
        case Text.stripPrefix prefix fieldName of
          Just stripped -> applyFirst toLower stripped
          Nothing -> fieldName
    in M1 <$> updateFromConfigG @inner (key /. Path [scopedKey]) config inner

  fetchFromConfigWithConNameG s key config =
    let
      applyFirst :: (Char -> Char) -> Text -> Text
      applyFirst f t = case Text.uncons t of
        Just (c, ts) -> Text.cons (f c) ts
        Nothing -> t

      fieldName = Text.pack $ selName @selector undefined
      prefix = applyFirst toLower $ Text.pack s
      scopedKey =
        case Text.stripPrefix prefix fieldName of
          Just stripped -> applyFirst toLower stripped
          Nothing -> fieldName
    in fmap M1 <$> fetchFromConfigG @inner (key /. Path [scopedKey]) config

-- | Purely 'Generics' machinery, ignore...
instance (UpdateFromConfig inner) => UpdateFromConfigG (Rec0 inner) where
  updateFromConfigG key config (K1 inner) = do
    K1 <$> updateFromConfig @inner key config inner
  fetchFromConfigG key config = do
    fmap K1 <$> fetchFromConfig @inner key config
