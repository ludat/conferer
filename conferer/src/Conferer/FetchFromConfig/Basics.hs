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

fetchFromConfigByRead :: Read a => Key -> Config -> IO (Maybe a)
fetchFromConfigByRead = fetchFromConfigWith (readMaybe . Text.unpack)

fromValueWith :: (Text -> Maybe a) -> Text -> Maybe a
fromValueWith parseValue valueAsText = parseValue valueAsText

fetchFromConfigWith :: (Text -> Maybe a) -> Key -> Config -> IO (Maybe a)
fetchFromConfigWith parseValue key config = do
  value <- getKey key config
  case value of
    Just x -> return $ Just $ fromMaybe (error "muerte") $ fromValueWith parseValue x
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

instance UpdateFromConfigG b => UpdateFromConfigG (D1 a b) where
  updateFromConfigG key config (M1 a) =
    M1 <$> updateFromConfigG key config a

instance (UpdateFromConfigWithConNameG b, Constructor a) => UpdateFromConfigG (C1 a b) where
  updateFromConfigG key config (M1 a) =
    M1 <$> updateFromConfigWithConNameG @b (conName @a undefined) key config a

class UpdateFromConfigWithConNameG f where
  updateFromConfigWithConNameG :: String -> Key -> Config -> f a -> IO (f a)

instance (UpdateFromConfigWithConNameG a, UpdateFromConfigWithConNameG b) => UpdateFromConfigWithConNameG (a :*: b) where
  updateFromConfigWithConNameG s key config (a :*: b) = do
    c1 <- updateFromConfigWithConNameG @a s key config a
    c2 <- updateFromConfigWithConNameG @b s key config b
    return (c1 :*: c2)

applyFirst :: (Char -> Char) -> Text -> Text
applyFirst f t = case Text.uncons t of
  Just (c, ts) -> Text.cons (f c) ts
  Nothing -> t

instance (UpdateFromConfigG b, Selector a) => UpdateFromConfigWithConNameG (S1 a b) where
  updateFromConfigWithConNameG s key config (M1 j) =
    let
      fieldName = Text.pack $ selName @a undefined
      prefix = applyFirst toLower $ Text.pack  s
      scopedKey =
        case Text.stripPrefix prefix fieldName of
          Just stripped -> applyFirst toLower stripped
          Nothing -> fieldName
    in M1 <$> updateFromConfigG @b (key /. Path [scopedKey]) config j

instance (FetchFromConfig a, Show a) => UpdateFromConfigG (Rec0 a) where
  updateFromConfigG key config (K1 j) = do
    coso <- fetch @a key config
    case coso of
      Just a -> return $ K1 a
      Nothing -> return $ K1 j
