{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.Basics where

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

updateAllAtOnceUsingFetch :: forall a. (FromConfig a, Typeable a) => Key -> Config -> a -> IO a
updateAllAtOnceUsingFetch key config old = do
  fetchFromConfig key config
    >>= \case 
      Just new -> return new
      Nothing -> do
        evaluate $ mapException (\(e :: FailedToFetchError) -> keyNotPresentError key (Proxy :: Proxy a)) $ old

instance FromConfig Int where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigByRead

instance FromConfig Integer where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigByRead

instance FromConfig Float where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigByRead

instance FromConfig ByteString where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigWith (Just . Text.encodeUtf8)

instance DefaultConfig (Maybe a) where
  configDef = Nothing
instance (FromConfig a) => FromConfig (Maybe a) where
  updateFromConfig k config (Just a) = do
    res <- updateFromConfig k config a
    Just <$> evaluate res
  updateFromConfig k config Nothing = do
    fetchFromConfig k config
  fetchFromConfig k config = do
    fetchFromConfig @a k config
      >>= \case
        Just res -> Just <$> Just <$> evaluate res
        Nothing -> return $ Just Nothing


instance FromConfig String where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigWith (Just . Text.unpack)

instance FromConfig Text where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigWith Just


instance FromConfig Bool where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigWith parseBool

parseBool text =
  case Text.toLower text of
    "false" -> Just False
    "true" -> Just True
    _ -> Nothing

updateFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> a -> IO (a)
updateFromConfigByRead = updateFromConfigWith (readMaybe . Text.unpack)

updateFromConfigByIsString :: (Typeable a, IsString a) => Key -> Config -> a -> IO (a)
updateFromConfigByIsString = updateFromConfigWith (Just . fromString . Text.unpack)

fetchFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> IO (Maybe a)
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

fetchFromConfigByIsString :: (Typeable a, IsString a) => Key -> Config -> IO (Maybe a)
fetchFromConfigByIsString = fetchFromConfigWith (Just . fromString . Text.unpack)

fromValueWith :: (Text -> Maybe a) -> Text -> Maybe a
fromValueWith parseValue valueAsText = parseValue valueAsText

fetchFromConfigWith :: forall a. Typeable a => (Text -> Maybe a) -> Key -> Config -> IO (Maybe a)
fetchFromConfigWith parseValue key config = do
  getKey key config >>=
    \case
      Just value ->
        return $ Just $ 
          fromMaybe (throw $ ConfigParsingError key value (typeRep (Proxy :: Proxy a))) $
          fromValueWith parseValue value
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
  FromConfig newvalue
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

instance FromConfigG inner =>
    FromConfigG (D1 metadata inner) where
  updateFromConfigG key config (M1 inner) =
    M1 <$> updateFromConfigG key config inner
  fetchFromConfigG key config =
    fmap M1 <$> fetchFromConfigG key config

instance (FromConfigWithConNameG inner, Constructor constructor) =>
    FromConfigG (C1 constructor inner) where
  updateFromConfigG key config (M1 inner) =
    M1 <$> updateFromConfigWithConNameG @inner (conName @constructor undefined) key config inner
  fetchFromConfigG key config =
    fmap M1 <$> fetchFromConfigWithConNameG @inner (conName @constructor undefined) key config

class FromConfigWithConNameG f where
  updateFromConfigWithConNameG :: String -> Key -> Config -> f a -> IO (f a)
  fetchFromConfigWithConNameG :: String -> Key -> Config -> IO (Maybe (f a))

instance (FromConfigWithConNameG left, FromConfigWithConNameG right) =>
    FromConfigWithConNameG (left :*: right) where
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

instance (FromConfigG inner, Selector selector) =>
    FromConfigWithConNameG (S1 selector inner) where
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
instance (FromConfig inner) => FromConfigG (Rec0 inner) where
  updateFromConfigG key config (K1 inner) = do
    K1 <$> updateFromConfig @inner key config inner
  fetchFromConfigG key config = do
    fmap K1 <$> fetchFromConfig @inner key config
