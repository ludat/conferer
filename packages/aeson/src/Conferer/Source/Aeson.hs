-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for json config files using Aeson
{-# LANGUAGE TypeApplications #-}
module Conferer.Source.Aeson where

import Data.Aeson
import Control.Applicative
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector ((!?))
import qualified Data.Vector as Vector
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List
import System.Directory
import Control.Exception
import Control.Monad (guard)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Conferer.Source.Files
import qualified Conferer.Source.Null as Null
import Conferer.Source

-- | 'Source' that read a config file as json and uses that value in a way that
-- makes sense for Conferer but doesn't respect json perfectly.
data JsonSource = JsonSource
  { value :: Value
  , filepath :: FilePath
  } deriving (Show, Eq)

instance IsSource JsonSource where
  getKeyInSource JsonSource {..} key = do
    return $ valueToText =<< traverseJSON key value
  getSubkeysInSource JsonSource {..} key = do
    return $ fmap (key /.) $ maybe [] listKeysInJSON $ traverseJSON key value
  explainNotFound JsonSource {..} key  =
    renderExplainForAeson filepath $ getNoKeyExplainResult key value
  explainSettedKey JsonSource {..} key =
    concat
    [ "json key '"
    , showRawKey $ getRealRawKey key value
    , "' on file '"
    , filepath
    , "'"
    ]
    where
      getRealRawKey :: Key -> Value -> RawKey
      getRealRawKey k originalValue =
        case getNoKeyExplainResult k originalValue of
          FoundIn rawKey -> rawKey
          result@(MissingKeyInPlainObject _ _ _) ->
            error $ "Getting an existing key returned that it doesn't exist, \
                    \that is a bug in conferer, please report it at \
                    \https://github.com/ludat/conferer/issues with :\n " ++ show result

renderExplainForAeson :: FilePath -> ExplainResult -> String
renderExplainForAeson filepath e =
  case e of
    MissingKeyInPlainObject existingPath nonExistingPath v ->
      "Replacing " ++
      showRawKeyAsTarget existingPath ++
      " from '" ++
      value2String v ++
      "' to '" ++
      value2String (merge v $ objectFromPath nonExistingPath (String "some value")) ++
      "' on file '" ++ filepath ++ "'"
    result@(FoundIn _) ->
      error $ "Getting an non existant key returned that it exists, \
              \that is a bug in conferer, please report it at \
              \https://github.com/ludat/conferer/issues with :\n " ++ show result
  where
    showRawKeyAsTarget :: RawKey -> String
    showRawKeyAsTarget [] = "the whole json"
    showRawKeyAsTarget k = "the value at '" ++ showRawKey k ++ "'"

merge :: Value -> Value -> Value
merge v1 v2 =
  let o1 = value2map v1
      o2 = value2map v2
      newO = HashMap.union o1 o2
  in
    -- If the new object only has "_self" key we can replace it with the contents
    -- of self
    case (HashMap.size newO, HashMap.lookup "_self" newO) of
      (1, Just v) -> v
      _ -> Object newO
  where
    value2map :: Value -> HashMap Text Value
    value2map (Object o) = o
    value2map (Array v) =
      HashMap.fromList $ zip (fmap (Text.pack . show @Integer) [0..]) $ Vector.toList v
    value2map v = HashMap.singleton "_self" v


objectFromPath :: RawKey -> Value -> Value
objectFromPath [] content = content
objectFromPath (k:ks) content =
  Object $ HashMap.fromList [(k, objectFromPath ks content)]

-- | Utility function to show a 'Value'
value2String :: Value -> String
value2String = LBS.unpack . encode

-- | Utility function to show a raw key to the user
showRawKey :: RawKey -> String
showRawKey rawKey =
  (intercalate "." $ fmap Text.unpack $ rawKey)

-- | This is the representation of the 'explainNotFound', this is useful
-- for sources which are not actually json but are isomorphic with json
-- like dhall or yaml.
data ExplainResult
  = MissingKeyInPlainObject
      RawKey
      -- ^ The path that right now is present in the actual config
      RawKey
      -- ^ The path that is missing in the object
      Value
      -- ^ The current value that is present in the config
  | FoundIn RawKey
  deriving (Eq)

instance Show ExplainResult where
  show (MissingKeyInPlainObject alreadyPresentPath missingPath currentValue) =
    intercalate " "
      [ "MissingKeyInPlainObject"
      , show alreadyPresentPath
      , show missingPath
      , "[aesonQQ|" ++ value2String currentValue ++ "|]"
      ]
  show (FoundIn rawKey) =
    intercalate " "
      [ "MissingKeyInPlainObject"
      , show rawKey
      ]

getNoKeyExplainResult :: Key -> Value -> ExplainResult
getNoKeyExplainResult key value = go ([], rawKeyComponents key) value
  where
    go :: (RawKey, RawKey) -> Value -> ExplainResult
    go (visitedKeys, []) v@(Object o) =
      case HashMap.lookup "_self" o of
        Just innerValue@(Object _) ->
          MissingKeyInPlainObject (visitedKeys ++ ["_self"]) [] innerValue

        Just innerValue@(Array _) ->
          MissingKeyInPlainObject (visitedKeys ++ ["_self"]) [] innerValue

        Just _v -> FoundIn (visitedKeys ++ ["_self"])

        Nothing ->
          MissingKeyInPlainObject visitedKeys [] v

    go (visitedKeys, []) v@(Array _) = MissingKeyInPlainObject visitedKeys [] v
    go (visitedKeys, []) _v = FoundIn visitedKeys
    go (visitedKeys, k:ks) v =
      case v of
        (Object o) ->
          case HashMap.lookup k o of
            Nothing -> MissingKeyInPlainObject visitedKeys (k:ks) v
            (Just x) -> go (visitedKeys ++ [k], ks) x
        (Array vs) ->
          case readMaybe $ Text.unpack k of
            Nothing -> MissingKeyInPlainObject visitedKeys (k:ks) v
            Just (n :: Int) ->
              case vs !? n of
                Nothing ->
                  MissingKeyInPlainObject visitedKeys (k:ks) v
                Just innerV ->
                  go (visitedKeys ++ [k], ks) innerV

        _ ->
          MissingKeyInPlainObject visitedKeys (k:ks) v

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
fromFilePath' relativeFilePath = do
  fileToParse <- makeAbsolute relativeFilePath
  fileExists <- doesFileExist fileToParse
  if fileExists
    then do
      value <- decodeStrict' <$> B.readFile fileToParse
      case value of
        Nothing ->
          error $ "Failed to decode json file '" ++ fileToParse ++ "'"
        Just v -> do
          case invalidJsonKeys v of
            [] ->
              return $ fromValue fileToParse v
            errors ->
              throwIO $ JsonHasInvalidKeysError fileToParse errors
    else do
      return $ Source $ Null.NullSource
        { nullExplainNotFound = \key ->
          concat
          [ "Creating a file '"
          , fileToParse
          , "' (it doesn't exist now) with the object '"
          , value2String $ objectFromPath
              (rawKeyComponents key)
              (String "some value")
          , "'"
          ]
        }

-- | Exception thrown from 'fromFilePath' when the json in the
-- parsed file has incorrect keys
data JsonHasInvalidKeysError =
  JsonHasInvalidKeysError FilePath [RawKey] deriving (Eq, Show)

instance Exception JsonHasInvalidKeysError

-- | Create a 'Source' from a json value, never fails.
fromValue :: FilePath -> Value -> Source
fromValue filepath value =
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
              sort $
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
      (_, Object o) ->
        let
          self =
            case valueToText <$> HashMap.lookup "_self" o of
              Just _ -> [key]
              Nothing -> []
        in self ++ do
          (k, v) <- HashMap.toList o
          guard $ isValidKeyFragment k
          go (key /. fromText k) v
      (_, Array as) -> do
        (index :: Integer, v) <- zip [0..] $ Vector.toList as
        go (key /. mkKey (show index)) v
      (Nothing, _) -> []
      (_, _) -> [key]

-- | Turn json 'Value' into 'Text' to return that key
valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText (Object o) = do
  selfValue <- HashMap.lookup "_self" o
  valueToText selfValue
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

type RawKey = [Text]

-- | Validates that a json has the correct format for keys,
-- since Conferer 'Key's are pretty restricted.
--
-- The Source will work with incorrect keys but they will
-- be ignored.
invalidJsonKeys :: Value -> [RawKey]
invalidJsonKeys = filter (not . validKey) . allKeys
  where
    validFragmentForJSON :: Text -> Bool
    validFragmentForJSON fragment = isValidKeyFragment fragment || fragment == "_self"
    validKey :: RawKey -> Bool
    validKey fragments = all validFragmentForJSON fragments

-- | Returns all keys in a json object
allKeys :: Value -> [RawKey]
allKeys = go mempty
  where
    go :: RawKey -> Value -> [RawKey]
    go rawkey value =
      case value of
        Object o ->
          let
            keys =
              fmap (\t -> rawkey ++ [t])
              . HashMap.keys
              $ o
          in keys ++ do
          (k, v) <- HashMap.toList o
          let subkey = rawkey ++ [k]
          go subkey v
        Array as -> do
          (index :: Integer, v) <- zip [0..] $ Vector.toList as
          let subkey = rawkey ++ [Text.pack $ show index]
          go subkey v
        _ -> []
