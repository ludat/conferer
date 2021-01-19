-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for json config files using Aeson
{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Aeson where

import Data.Aeson
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector ((!?))
import qualified Data.Vector as Vector
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import System.Directory (doesFileExist)

import Conferer.Source.Files
import qualified Conferer.Source.Null as Null
import Conferer.Source

-- | 'Source' that read a config file as json and uses that value in a way that
-- makes sense for Conferer but doesn't respect json perfectly.
data JsonSource = JsonSource
  { value :: Value
  } deriving (Show, Eq)

instance IsSource JsonSource where
  getKeyInSource JsonSource {..} key = do
    return $ valueToText =<< traverseJSON key value
  getSubkeysInSource JsonSource {..} key = do
    return $ fmap (key /.) $ maybe [] listKeysInJSON $ traverseJSON key value

-- | Create a 'SourceCreator' which uses files with @config/{env}.json@
-- template and then uses 'fromFilePath'
fromConfig :: Key -> SourceCreator
fromConfig key config = do
  fileToParse <- getFilePathFromEnv key "json" config
  fromFilePath' fileToParse

-- | Create a 'SourceCreator' from a filepath
--
-- If the file is not present it will behave as if it had no keys.
--
-- If the file doesn't have valid json it will throw an error.
fromFilePath :: FilePath -> SourceCreator
fromFilePath fileToParse _config =
  fromFilePath' fileToParse

-- | Create a 'Source' from a filepath
--
-- If the file is not present it will behave as if it had no keys.
--
-- If the file doesn't have valid json it will throw an error.
fromFilePath' :: FilePath -> IO Source
fromFilePath' fileToParse = do
  fileExists <- doesFileExist fileToParse
  if fileExists
    then do
      value <- decodeStrict' <$> B.readFile fileToParse
      case value of
        Nothing ->
          error $ "Failed to decode json file '" ++ fileToParse ++ "'"
        Just v -> do
          return $ fromValue v
    else do
      return $ Null.empty

-- | Create a 'Source' from a json value, never fails.
fromValue :: Value -> Source
fromValue value =
  Source JsonSource {..}

-- | Traverse a 'Value' using a 'Key' to get a 'Value'.
--
-- This function can nest objects and arrays when keys are nested
--
-- @
-- 'traverseJSON' "a.b" {a: {b: 12}} == Just "12"
-- 'traverseJSON' "a.b" {a: {b: false}} == Just "false"
-- 'traverseJSON' "a" {a: {b: false}} == Nothing
-- 'traverseJSON' "1" [false, true] == Just "true"
-- 'traverseJSON' "0.a" [{a: "hi"}] == Just "hi"
-- 'traverseJSON' "0" [] == Nothing
-- @
traverseJSON :: Key -> Value -> Maybe Value
traverseJSON key value =
 case (unconsKey key, value) of
   (Nothing, v) ->
     Just v
   (Just ("keys", ""), Object o) ->
      HashMap.lookup "keys" o
        <|> pure (
              String $
              mconcat $
              intersperse "," $
              HashMap.keys o)
   (Just (c, ks), Object o) ->
     HashMap.lookup c o >>= traverseJSON ks
   (Just ("keys", ""), Array vs) ->
      Just $
        String $
        mconcat $
        intersperse "," $
        fmap (Text.pack . show)
        [0..length vs - 1]
   (Just (c, ks), Array vs) -> do
     n :: Int <- readMaybe $ Text.unpack c
     v <- vs !? n
     traverseJSON ks v
   (Just _, _) ->
     Nothing

-- | Get the list of available keys inside a json value
listKeysInJSON :: Value -> [Key]
listKeysInJSON = go ""
  where
  go :: Key -> Value -> [Key]
  go key value =
    case (unconsKey key, value) of
      (_, Object o) -> do
        (k, v) <- HashMap.toList o
        go (key /. fromText k) v
      (_, Array as) -> do
        (index :: Integer, v) <- zip [0..] $ Vector.toList as
        go (key /. mkKey (show index)) v
      (Nothing, _) -> []
      (_, _) -> [key]

-- | Turn json 'Value' into 'Text' to return that key
valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText (Object _o) = Nothing
valueToText (Array _as) = Nothing
valueToText (Number n) = Just $ Text.decodeUtf8 $ L.toStrict $ encode $ Number n
valueToText (Bool b) = Just $ boolToString b
valueToText (Null) = Nothing

-- | Turn a 'GHC.Types.Bool' into a 'Text'
boolToString :: Bool -> Text
boolToString True = "true"
boolToString False = "false"

-- | Because we use an old version of aeson
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Error _) = Nothing
resultToMaybe (Success a) = Just a

