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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List
import System.Directory
import Control.Exception
import Control.Monad (guard, unless)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Conferer.Source.Files
import qualified Conferer.Source.Null as Null
import Conferer.Source
import Data.Maybe

-- | 'Source' that read a config file as json and uses that value in a way that
-- makes sense for Conferer but doesn't respect json perfectly.
data JsonSource = JsonSource
  { value :: ValueIR
  , filepath :: FilePath
  } deriving (Show, Eq)

instance IsSource JsonSource where
  getKeyInSource JsonSource {..} key = do
    case getNoKeyExplainResult key value of
      FoundConcrete _ t _ -> return $ Just t
      _ -> return Nothing

  getSubkeysInSource JsonSource {..} key = do
    case traverseJSON key value of
      Just v ->
        pure $ (key /.) <$> listKeysInJSON v
      Nothing ->
        pure []

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
      getRealRawKey :: Key -> ValueIR -> RawKey
      getRealRawKey k originalValue =
        case getNoKeyExplainResult k originalValue of
          FoundConcrete rawKey _ _ -> rawKey
          result ->
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
      value2String (setKey "some value" nonExistingPath v) ++
      "' on file '" ++ filepath ++ "'"
    result@FoundConcrete {} ->
      error $ "Getting an non existant key returned that it exists, \
              \that is a bug in conferer, please report it at \
              \https://github.com/ludat/conferer/issues with :\n " ++ show result
  where
    showRawKeyAsTarget :: RawKey -> String
    showRawKeyAsTarget [] = "the whole json"
    showRawKeyAsTarget k = "the value at '" ++ showRawKey k ++ "'"

setKey :: Text -> RawKey -> ValueIR -> ValueIR
setKey newValue = go
  where
  go :: RawKey -> ValueIR -> ValueIR
  go [] (ObjectIR _ o) =
    if HashMap.null o
      then RawValue newValue $ String newValue
      else ObjectIR (Just (newValue, String newValue)) o
  go (k:ks) (ObjectIR v o) = ObjectIR v $ HashMap.alter
    (\case
      Just oldIR -> Just $ go ks oldIR
      Nothing -> Just $ go ks (ObjectIR Nothing HashMap.empty)
    ) k o

  go [] (RawValue _ _) = RawValue newValue $ String newValue
  go (k:ks) (RawValue t v) =
    -- FIXME: this is a hack OR IS IT?
    ObjectIR (Just (t, v)) $ HashMap.singleton k $ go ks $ ObjectIR Nothing HashMap.empty

  go [] (ArrayIR vs) =
    if Vector.null vs
      then RawValue newValue $ String newValue
      else ObjectIR (Just (newValue, String newValue)) $ list2object vs
  go (k:ks) (ArrayIR vs) =
    case readMaybe @Int $ Text.unpack k of
      Just n | isJust $ vs !? n ->
        ArrayIR $ Vector.imap (\i v -> if i == n then go ks v else v) vs
      _ ->
        ObjectIR Nothing $
          HashMap.insert k (go ks (ObjectIR Nothing HashMap.empty)) $ list2object vs

  list2object = HashMap.fromList . zip (fmap (Text.pack . show @Integer) [0..]) . Vector.toList

fromRight :: Either e a -> a
fromRight (Right a) = a
fromRight (Left _) = error "f"

-- objectFromPath :: RawKey -> ValueIR -> ValueIR
-- objectFromPath [] content = content
-- objectFromPath (k:ks) content =
--   ObjectIR Nothing $ HashMap.fromList [(k, objectFromPath ks content)]

-- | Utility function to show a 'Value'
value2String :: ValueIR -> String
value2String = LBS.unpack . encode . valueIR2Value

valueIR2Value :: ValueIR -> Value
valueIR2Value = go
  where
  go :: ValueIR -> Value
  go (RawValue _ v) = v
  go (ArrayIR vs) = Array $ fmap go vs
  go (ObjectIR j o) =
    let
      initialMap =
        case j of
          Just (_, v) -> HashMap.singleton "_self" v
          Nothing -> HashMap.empty

    in Object $ HashMap.union initialMap (fmap go o)

-- | Utility function to show a raw key to the user
showRawKey :: RawKey -> String
showRawKey =
  intercalate "." . fmap Text.unpack

-- | This is the representation of the 'explainNotFound', this is useful
-- for sources which are not actually json but are isomorphic with json
-- like dhall or yaml.
data ExplainResult
  = MissingKeyInPlainObject
      RawKey
      -- ^ The path that right now is present in the actual config
      RawKey
      -- ^ The path that is missing in the object
      ValueIR
      -- ^ The current value that is present in the config
  -- | FoundCompundThing RawKey ValueIR
  | FoundConcrete RawKey Text ValueIR
  deriving (Eq)

instance Show ExplainResult where
  show (MissingKeyInPlainObject alreadyPresentPath missingPath currentValue) =
    unwords
      [ "MissingKeyInPlainObject"
      , show alreadyPresentPath
      , show missingPath
      , show currentValue
      ]
  show (FoundConcrete rawKey rawValue actualValue) =
    unwords
      [ "FoundConcrete"
      , show rawKey
      , Text.unpack rawValue
      , show actualValue
      ]

getNoKeyExplainResult :: Key -> ValueIR -> ExplainResult
getNoKeyExplainResult key = go ([], rawKeyComponents key)
  where
  go :: (RawKey, RawKey) -> ValueIR -> ExplainResult
  go (visitedKeys, []) value@(ObjectIR maybeSelf _) =
    case maybeSelf of
      Just (t, _) ->
        FoundConcrete (visitedKeys ++ ["_self"]) t value

      Nothing ->
        MissingKeyInPlainObject visitedKeys [] value

  go (visitedKeys, []) v@(ArrayIR _) =
    MissingKeyInPlainObject visitedKeys [] v

  go (visitedKeys, []) v@(RawValue t _) =
    FoundConcrete visitedKeys t v

  go (visitedKeys, ["keys"]) value@(ObjectIR _ o) =
    case HashMap.lookup "keys" o of
      Just v ->
        go (visitedKeys ++ ["keys"], []) v
      Nothing ->
        let
          generatedKeys =
            mconcat $
            intersperse "," $
            sort $
            HashMap.keys o
        in FoundConcrete (visitedKeys ++ ["keys"]) generatedKeys value

  go (visitedKeys, k:ks) value@(ObjectIR _ o) =
    case HashMap.lookup k o of
      Just innerValue -> go (visitedKeys ++ [k], ks) innerValue
      Nothing -> MissingKeyInPlainObject visitedKeys (k:ks) value
  go (visitedKeys, ["keys"]) value@(ArrayIR vs) =
    let
      generatedKeys =
        mconcat $
        intersperse "," $
        fmap (Text.pack . show)
        [0..length vs - 1]
    in FoundConcrete visitedKeys generatedKeys value
  go (visitedKeys, k:ks) v@(ArrayIR vs) =
    case readMaybe $ Text.unpack k of
      Nothing -> MissingKeyInPlainObject visitedKeys (k:ks) v
      Just (n :: Int) ->
        case vs !? n of
          Nothing ->
            MissingKeyInPlainObject visitedKeys (k:ks) v
          Just innerV ->
            go (visitedKeys ++ [k], ks) innerV

  go (visitedKeys, futureKeys) v =
    MissingKeyInPlainObject visitedKeys futureKeys v

data ValueIR
  = RawValue Text Value
  | ArrayIR (Vector ValueIR)
  | ObjectIR (Maybe (Text, Value)) (HashMap Text ValueIR)
  deriving (Eq, Show)

aaaa :: Value -> Either String ValueIR
aaaa = go
  where
  go (Object o) = do
    let invalidKeys = filter (not . validFragmentForJSON) $ HashMap.keys o
    unless (null invalidKeys) $
      Left $ "keys malosas: " ++ show invalidKeys
    self <- parseSelf $ HashMap.lookup "_self" o
    content <- traverse go o
    Right $ ObjectIR self content
  go (Array as) = do
    content <- traverse go as
    Right $ ArrayIR content
  go v@(Bool b) =
    Right $ RawValue (boolToString b) v
  go v@(Number n) =
    Right $ RawValue (numberToString n) v
  go v@Null =
    Right $ RawValue "null" v
  go v@(String s) =
    Right $ RawValue s v

  parseSelf :: Maybe Value -> Either String (Maybe (Text, Value))
  parseSelf =
    \case
      Just v@(String t) -> Right $ Just (t, v)
      Just v@(Object _) -> Left $ "'_self' should be a primitive (number, string or bool) but it is: '" ++ LBS.unpack (encode v) ++ "'"
      Just v@(Array _) -> Left $ "'_self' should be a primitive (number, string or bool) but it is: '" ++ LBS.unpack (encode v) ++ "'"
      Just v@(Number n) -> Right $ Just (numberToString n, v)
      Just v@(Bool b) -> Right $ Just (boolToString b, v)
      Just v@Null -> Right $ Just ("null", v)
      Nothing -> Right Nothing

  boolToString True = "true"
  boolToString False = "false"

  numberToString = Text.decodeUtf8 . L.toStrict . encode . Number

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
      decodeStrict' <$> B.readFile fileToParse
      >>= \case
        Nothing ->
          error $ "Failed to decode json file '" ++ fileToParse ++ "'"
        Just v -> do
          fromValue fileToParse v
    else do
      return $ Source $ Null.NullSource
        { nullExplainNotFound = \key ->
          concat
          [ "Creating a file '"
          , fileToParse
          , "' (it doesn't exist now) with the object '"
          , value2String $ setKey "some text"
              (rawKeyComponents key)
              (ObjectIR Nothing HashMap.empty)
          , "'"
          ]
        }

-- | Exception thrown from 'fromFilePath' when the json in the
-- parsed file has incorrect keys
data JsonHasInvalidKeysError =
  JsonHasInvalidKeysError FilePath [RawKey] deriving (Eq, Show)

instance Exception JsonHasInvalidKeysError

-- | Create a 'Source' from a json value, never fails.
fromValue :: FilePath -> Value -> IO Source
fromValue filepath rawValue =
  case fromValue' filepath rawValue of
    Right s ->
      pure s
    Left e ->
      throwIO $ JsonHasInvalidKeysError e []

-- | Create a 'Source' from a json value, never fails.
fromValue' :: FilePath -> Value -> Either String Source
fromValue' filepath rawValue =
  case aaaa rawValue of
    Right value ->
      Right $ Source JsonSource {..}
    Left e ->
      Left e

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
traverseJSON :: Key -> ValueIR -> Maybe ValueIR
traverseJSON key value =
  case getNoKeyExplainResult key value of
    FoundConcrete _ _ v -> Just v
    MissingKeyInPlainObject _ [] v -> Just v
    MissingKeyInPlainObject {} -> Nothing

-- | Get the list of available keys inside a json value
listKeysInJSON :: ValueIR -> [Key]
listKeysInJSON = go ""
  where
  go :: Key -> ValueIR -> [Key]
  go key value =
    case (unconsKey key, value) of
      (_, ObjectIR x o) ->
        let
          self =
            case x of
              Just _ -> [key]
              Nothing -> []
        in self ++ do
          (k, v) <- HashMap.toList o
          guard $ isValidKeyFragment k
          go (key /. fromText k) v
      (_, ArrayIR as) -> do
        (index :: Integer, v) <- zip [0..] $ Vector.toList as
        go (key /. mkKey (show index)) v
      (Nothing, _) -> []
      (_, _) -> [key]

type RawKey = [Text]

validFragmentForJSON :: Text -> Bool
validFragmentForJSON fragment = isValidKeyFragment fragment || fragment == "_self"
