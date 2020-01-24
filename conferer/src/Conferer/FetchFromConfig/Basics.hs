{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Conferer.FetchFromConfig.Basics where

import           Conferer.Types
import           Conferer.Core (getKey, (/.))
import           Control.Monad (join)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Control.Exception
import           Data.Char (toLower)
import           Data.Typeable (Typeable, typeRep, Proxy(..))
import           GHC.Generics

import           Data.String (IsString, fromString)

import           Text.Read (readMaybe)

instance FetchFromConfig Int where
  fetch = fetchFromConfigByRead

instance FetchFromConfig Integer where
  fetch = fetchFromConfigByRead

instance FetchFromConfig Float where
  fetch = fetchFromConfigByRead

instance FetchFromConfig ByteString where
  fetch = fetchFromConfigWith (Just . Text.encodeUtf8)

instance FetchFromConfig a => FetchFromConfig (Maybe a)  where
  fetch k config =
    fmap return <$> fetch k config

instance FetchFromConfig String where
  fetch = fetchFromConfigWith (Just . Text.unpack)

instance FetchFromConfig Text where
  fetch = fetchFromConfigWith (Just)

instance FetchFromConfig Bool where
  fetch = fetchFromConfigWith parseBool
      where
        parseBool text =
          case Text.toLower text of
            "false" -> Just False
            "true" -> Just True
            _ -> Nothing

fetchFromConfigByRead :: (Typeable a, Read a) => Key -> Config -> IO (Maybe a)
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

fromValueWith :: (Text -> Maybe a) -> Text -> Maybe a
fromValueWith parseValue valueAsText = parseValue valueAsText

fetchFromConfigWith :: forall a. Typeable a => (Text -> Maybe a) -> Key -> Config -> IO (Maybe a)
fetchFromConfigWith parseValue key config = do
  getKey key config >>=
    \case
      Just value ->
        return $
          Just $
          fromMaybe (throw $ ConfigParsingError key value (typeRep (Proxy :: Proxy a))) $
          fromValueWith parseValue value
      Nothing -> return Nothing

-- | Concatenate many transformations to the config based on keys and functions
findKeyAndApplyConfig ::
  forall newvalue config.
  FetchFromConfig newvalue
  => Config -- ^ Complete config
  -> Key -- ^ Key that indicates the part of the config that we care about
  -> Key -- ^ Key that we use to find the config (usually concatenating with the
         -- other key)
  -> (newvalue -> config -> config) -- ^ Function that knows how to use the
                                    -- value to update the config
  -> config -- ^ Result of the last config updating
  -> IO config -- ^ Updated config
findKeyAndApplyConfig config k relativeKey f customConfig = do
  t <- fetch @newvalue (k /. relativeKey) config
  case t of
    Nothing -> return customConfig
    Just a -> return $ f a customConfig

instance UpdateFromConfigG inner =>
    UpdateFromConfigG (D1 metadata inner) where
  updateFromConfigG key config (M1 inner) =
    M1 <$> updateFromConfigG key config inner

instance (UpdateFromConfigWithConNameG inner, Constructor constructor) =>
    UpdateFromConfigG (C1 constructor inner) where
  updateFromConfigG key config (M1 inner) =
    M1 <$> updateFromConfigWithConNameG @inner (conName @constructor undefined) key config inner

class UpdateFromConfigWithConNameG f where
  updateFromConfigWithConNameG :: String -> Key -> Config -> f a -> IO (f a)

instance (UpdateFromConfigWithConNameG left, UpdateFromConfigWithConNameG right) =>
    UpdateFromConfigWithConNameG (left :*: right) where
  updateFromConfigWithConNameG s key config (left :*: right) = do
    leftValue <- updateFromConfigWithConNameG @left s key config left
    rightValue <- updateFromConfigWithConNameG @right s key config right
    return (leftValue :*: rightValue)

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

-- | Purely 'Generics' machinery, ignore...
instance (FetchFromConfig inner) => UpdateFromConfigG (Rec0 inner) where
  updateFromConfigG key config (K1 inner) = do
    fetch @inner key config
      >>= \case
            Just newInner -> return $ K1 newInner
            Nothing -> return $ K1 inner
