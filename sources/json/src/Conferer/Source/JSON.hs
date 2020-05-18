module Conferer.Source.JSON
  (
  -- * How to use this source
  -- | As any other source you can add it to a config using the 'addSource'
  -- function. There are a couple of oddities that come from supporting many
  -- different sources which do not support numbers, arrays or objects
  -- nativelly
  --
  -- @
  -- import 'Conferer'
  -- import Conferer.Source.JSON ('mkJsonSource')
  --
  -- main = do
  --   config <-
  --     'defaultConfig' \"awesomeapp\"
  --     & 'addSource' 'mkJsonSource'
  --   warpSettings <- 'getFromConfig' \"warp\" config
  --   runSettings warpSettings application
  -- @
  --
  -- This will result on a source that upon starting looks the file
  -- @config/{.env}.json@ in the current directory and uses it to provide config
  -- keys.
  --
  -- TODO Describe how we transform json into key value strings
  mkJsonSource
  , mkJsonSource'

  -- * Internal utility functions
  -- | These may be useful for someone but are subject to change at any point so
  -- use with care
  , traverseJSON
  , resultToMaybe
  , valueToText
  , boolToString
  )
where

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Vector
import           Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           System.Directory (doesFileExist)
import           Data.Monoid


import Conferer.Source.Files
import Conferer.Source.Null
import Conferer.Types

-- | Default 'SourceCreator' which usese files with @config/{env}.json@
-- template, if the file is not present it will behave like the null source
-- (it has no keys) but if the file doesn't have valid json it will throw an
-- error
mkJsonSource :: SourceCreator
mkJsonSource config = do
  fileToParse <- getFilePathFromEnv config "json"
  fileExists <- doesFileExist fileToParse
  if fileExists
    then do
      value <- decodeStrict' <$> B.readFile fileToParse
      case value of
        Nothing ->
          error $ "Failed to decode file '" <> fileToParse <> "'"
        Just v -> do
          mkJsonSource' v config
    else do
      mkNullSource config

-- | Just like 'mkJsonSource' but accepts the json value as a parameter
mkJsonSource' :: Value -> SourceCreator
mkJsonSource' v = \_config ->
  return $ Source
  { getKeyInSource = \k -> do
      return $ traverseJSON k v
  }

-- | Traverse a 'Value' using a 'Key' to get a value for conferer ('Text').
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
traverseJSON :: Key -> Value -> Maybe Text
traverseJSON (Path []) v = valueToText v
traverseJSON (Path (k:ks)) (Object o) =
  HashMap.lookup k o >>= traverseJSON (Path ks)
traverseJSON (Path (k:ks)) (Array vs) = do
  n :: Int <- readMaybe $ Text.unpack k
  value <- vs !? n
  traverseJSON (Path ks) value
traverseJSON (Path _) _ = Nothing

valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText (Object _o) = Nothing
valueToText (Array _as) = Nothing
valueToText (Number n) = Just $ Text.decodeUtf8 $ L.toStrict $ encode $ Number n
valueToText (Bool b) = Just $ boolToString b
valueToText (Null) = Nothing

boolToString :: Bool -> Text
boolToString True = "true"
boolToString False = "false"

-- | Because we use an old version of 'aeson'
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Error _) = Nothing
resultToMaybe (Success a) = Just a
