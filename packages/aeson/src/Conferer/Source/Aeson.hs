-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for json config files using Aeson
{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
module Conferer.Source.Aeson where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson hiding (Key)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import Data.Aeson
import qualified Data.HashMap.Strict as KeyMap
#endif
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
import Control.Monad (unless, forM)
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

-- | This represents a real json path (which has strictly more things than
-- a 'Key')
type RawKey = [Text]

-- | This is the representation of the 'explainNotFound', this is useful
-- for sources which are not actually json but are isomorphic with json
-- like dhall or yaml.
data FindKeyInValueResult
  = Missing
      RawKey
      -- ^ The path that right now is present in the actual config
      RawKey
      -- ^ The path that is missing in the object
      ValueIR
      -- ^ The current value that is present in the config
  | Found
      RawKey
      -- ^ The true json path where the key was found
      Text
      -- ^ The textual representation of the value
      ValueIR
      -- ^ The original json value that we found
  deriving (Eq, Show)

instance IsSource JsonSource where
  getKeyInSource JsonSource {..} key = do
    case findKeyInsideValue key value of
      Found _ t _ -> return $ Just t
      _ -> return Nothing

  getSubkeysInSource JsonSource {..} key = do
    let deepestValueFound = traverseJSON key value
        foundKey = valueIRKey deepestValueFound
    if foundKey == key
      then do
        pure $ listKeysInJSON deepestValueFound
      else do
        pure []

  explainNotFound JsonSource {..} key  =
    case findKeyInsideValue key value of
      Missing existingPath nonExistingPath v ->
        "Replacing " ++
        showRawKeyAsTarget existingPath ++
        " from '" ++
        valueIR2String v ++
        "' to '" ++
        valueIR2String (setKey "some value" nonExistingPath v) ++
        "' on file '" ++ filepath ++ "'"
      result@Found {} ->
        error $ "Getting an non existant key returned that it exists, \
                \that is a bug in conferer, please report it at \
                \https://github.com/ludat/conferer/issues with :\n " ++ show result
    where
      showRawKeyAsTarget :: RawKey -> String
      showRawKeyAsTarget [] = "the whole json"
      showRawKeyAsTarget k = "the value at '" ++ showRawKey k ++ "'"
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
        case findKeyInsideValue k originalValue of
          Found rawKey _ _ -> rawKey
          result ->
            error $ "Getting an existing key returned that it doesn't exist, \
                    \that is a bug in conferer, please report it at \
                    \https://github.com/ludat/conferer/issues with :\n " ++ show result


-- | set a 'RawKey' inside a 'ValueIR' while also preserving any already present
-- keys.
setKey :: Text -> RawKey -> ValueIR -> ValueIR
setKey newText = go
  where
  newValue = String newText
  go :: RawKey -> ValueIR -> ValueIR
  go [] (ObjectIR key _ o) =
    if HashMap.null o
      then RawValue key newText newValue
      else ObjectIR key (Just (newText, newValue)) o
  go (k:ks) (ObjectIR key v o) = ObjectIR key v $ HashMap.alter
    (\case
      Just oldIR -> Just $ go ks oldIR
      Nothing -> Just $ go ks (ObjectIR (key /. fromText k) Nothing HashMap.empty)
    ) k o

  go [] (RawValue key _ _) = RawValue key newText newValue
  go (k:ks) (RawValue key t v) =
    ObjectIR key (Just (t, v)) $ HashMap.singleton k $ go ks $ ObjectIR (key /. fromText k) Nothing HashMap.empty

  go [] (ArrayIR key vs) =
    if Vector.null vs
      then RawValue key newText newValue
      else ObjectIR key (Just (newText, newValue)) $ list2object vs
  go (k:ks) (ArrayIR key vs) =
    case readMaybe @Int $ Text.unpack k of
      Just n | isJust $ vs !? n ->
        ArrayIR key $ Vector.imap (\i v -> if i == n then go ks v else v) vs
      _ ->
        ObjectIR key Nothing $
          HashMap.insert k (go ks (ObjectIR (key /. fromText k) Nothing HashMap.empty)) $ list2object vs

  list2object = HashMap.fromList . zip (fmap (Text.pack . show @Integer) [0..]) . Vector.toList

-- | Utility function to show a 'Value'
value2String :: Value -> String
value2String = LBS.unpack . encode

-- | Utility function to show a 'ValueIR'
valueIR2String :: ValueIR -> String
valueIR2String = value2String . valueIR2Value

-- | Utility function like 'valueIRKey' but returns a 'RawKey' instead
valueIRRawKey :: ValueIR -> RawKey
valueIRRawKey = rawKeyComponents . valueIRKey

-- | Utility function get the 'Key' where the 'ValueIR' was found.
valueIRKey :: ValueIR -> Key
valueIRKey (RawValue k _ _) = k
valueIRKey (ArrayIR k _) = k
valueIRKey (ObjectIR k _ _) = k

-- | Utility function to get the original 'Value' back from a 'ValueIR'
--
-- This function satisfies that
-- @(valueIR2Value. fromRight . parseValue) == id@
valueIR2Value :: ValueIR -> Value
valueIR2Value = go
  where
  go :: ValueIR -> Value
  go (RawValue _ _ v) = v
  go (ArrayIR _ vs) = Array $ fmap go vs
  go (ObjectIR _ j o) =
    let
      initialMap =
        case j of
          Just (_, v) -> KeyMap.singleton "_self" v
          Nothing -> KeyMap.empty

#if MIN_VERSION_aeson(2,0,0)
    in Object $ KeyMap.union initialMap (KeyMap.fromHashMapText $ fmap go o)
#else
    in Object $ KeyMap.union initialMap (fmap go o)
#endif

-- | Utility function to show a raw key to the user
showRawKey :: RawKey -> String
showRawKey =
  intercalate "." . fmap Text.unpack

-- | Given a 'ValueIR' traverse it with a 'Key' to see if there is a value
-- for that specific 'Key'
--
-- This function implements the @keys@ special key
--
-- It works by traversing the 'ValueIR' using 'traverseJSON' which always
-- succeeds and then compares the key of the found 'ValueIR' with the
-- original key (checking for left overs), if there are none then that
-- value is returned, otherwise we try to use the special "keys" key
-- and finally it just returns a 'Missing' result
findKeyInsideValue :: Key -> ValueIR -> FindKeyInValueResult
findKeyInsideValue key aValue =
  let
    deepestValueFound = traverseJSON key aValue
    deepestKey = valueIRKey deepestValueFound
  in
    case (stripKeyPrefix deepestKey key, deepestValueFound) of
      (Just "keys", ObjectIR _ _ o) ->
        let generatedKeys =
              mconcat $
              intersperse "," $
              sort $
              HashMap.keys o
        in Found (rawKeyComponents key) generatedKeys deepestValueFound

      (Just "keys", ArrayIR _ vs) ->
        let
          generatedKeys =
            mconcat $
            intersperse "," $
            fmap (Text.pack . show)
            [0..length vs - 1]
        in Found (rawKeyComponents key) generatedKeys deepestValueFound

      (Just "", ObjectIR _ maybeSelf _) ->
        case maybeSelf of
          Just (t, _) ->
            Found (rawKeyComponents key ++ ["_self"]) t deepestValueFound
          Nothing ->
            Missing (rawKeyComponents key) [] deepestValueFound

      (Just "", ArrayIR _ _) ->
        Missing (rawKeyComponents key) [] deepestValueFound

      (Just "", RawValue _ t _) ->
        Found (rawKeyComponents key) t deepestValueFound

      (Just k, _) ->
        Missing (rawKeyComponents deepestKey) (rawKeyComponents k) deepestValueFound

      (Nothing, _) ->
        error $
          unlines
            [ "The impossible happened: deepestKey must be a suffix of key since \
              \one generates the other."
            , ""
            , "deepestKey: " ++ show deepestKey
            , "key: " ++ show key
            , "value: " ++ show aValue
            ]


-- | Internal Representation of a json value that matches the way we find keys
-- so concrete values are translated into a single constructor and the special
-- "_self" key is descriminated, and also each node contains its 'Key'
data ValueIR
  = ArrayIR
    -- ^ Repesentation of an array
    Key
    -- ^ path inside the original json
    (Vector ValueIR)
    -- ^ inner values
  | ObjectIR
    -- ^ Repesentation of an object
    Key
    -- ^ path inside the original json
    (Maybe (Text, Value))
    -- ^ "_self" key value, with the textual representation and the original value
    (HashMap Text ValueIR)
    -- ^ inner values (these Text are all valid key fragments)
  | RawValue Key Text Value
  deriving (Eq, Show)

-- | Parse a raw 'Value' into a nicer internal representation ('ValueIR')
-- and report errors if the json is malformed which currently may be
-- caused by keys with invalid charaters or _self key with invalid value.
parseValue :: Value -> Either ProblemWithJSON ValueIR
parseValue = go ""
  where
  go key (Object o) = do
    let
#if MIN_VERSION_aeson(2,0,0)
      hashmap = KeyMap.toHashMapText o
#else
      hashmap = o
#endif
      (validKeys, invalidKeys) = partition (isKeyFragment . fst)
        $ HashMap.toList
        $ HashMap.delete "_self" hashmap
    unless (null invalidKeys) $
      Left $ InvalidKey (rawKeyComponents key) (fst $ head invalidKeys)
    self <- sequence $ parseSelf key <$> HashMap.lookup "_self" hashmap
    content <- forM validKeys $ \(k, v) -> do
      let currentKey = key /. fromText k
      inner <- go currentKey v
      pure (k, inner)
    Right $ ObjectIR key self $ HashMap.fromList content

  go key (Array as) = do
    content <- forM (Vector.indexed as) $ \(i, v) -> do
      let
        k = Text.pack $ show i
        currentKey = key /. fromText k
      go currentKey v
    Right $ ArrayIR key content

  go key v@(Bool b) =
    Right $ RawValue key (boolToString b) v

  go key v@(Number n) =
    Right $ RawValue key (numberToString n) v

  go key v@Null =
    Right $ RawValue key "null" v

  go key v@(String s) =
    Right $ RawValue key s v

  parseSelf :: Key -> Value -> Either ProblemWithJSON (Text, Value)
  parseSelf k v =
    case v of
      (String t) -> Right (t, v)
      (Object _) -> Left $ InvalidSelfValue (rawKeyComponents k) v
      (Array _) -> Left $ InvalidSelfValue (rawKeyComponents k) v
      (Number n) -> Right (numberToString n, v)
      (Bool b) -> Right (boolToString b, v)
      Null -> Right ("null", v)

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
          , valueIR2String $ setKey "some text"
              (rawKeyComponents key)
              emptyRootObject
          , "'"
          ]
        }

-- | Utility function for creating an empty root object, mostly for
-- reporting errors
emptyRootObject :: ValueIR
emptyRootObject = ObjectIR "" Nothing HashMap.empty

-- | Exception thrown from 'fromFilePath' when the json in the
-- parsed file has incorrect keys
data InvalidJSONError =
  InvalidJSONError FilePath ProblemWithJSON
  deriving (Eq, Show)

-- | Possible problems while parsing a 'Value' into a 'ValueIR' in
-- 'parseValue'
data ProblemWithJSON
  = InvalidKey RawKey Text
  -- ^ a key has is not a valid key fragment (e.g. "k_e_y" or "")
  | InvalidSelfValue RawKey Value
  -- ^ a _self key has a non concrete value (like an object or an array)
  deriving (Eq, Show)

instance Exception InvalidJSONError

-- | Create a 'Source' from a json value, never fails.
fromValue :: FilePath -> Value -> IO Source
fromValue filepath rawValue =
  case fromValue' filepath rawValue of
    Right s ->
      pure s
    Left e ->
      throwIO $ InvalidJSONError filepath e

-- | Create a 'Source' from a json value, never fails.
fromValue' :: FilePath -> Value -> Either ProblemWithJSON Source
fromValue' filepath rawValue = do
  value <- parseValue rawValue
  pure $ Source JsonSource {..}

-- | Traverse a 'ValueIR' using a 'Key' to get the deepest 'ValueIR' that
-- matches that 'Key'
--
-- This means it's is possible that the value returned is not the value at
-- that 'Key'.
traverseJSON :: Key -> ValueIR -> ValueIR
traverseJSON = go
  where
  go :: Key -> ValueIR -> ValueIR
  go key value =
    case (unconsKey key, value) of
      (Just (k, restOfKey), ObjectIR _ _ o) -> fromMaybe value $ do
        x <- HashMap.lookup k o
        Just $ go restOfKey x
      (Just (k, restOfKey), ArrayIR _ vs) -> fromMaybe value $ do
        n <- readMaybe @Int $ Text.unpack k
        x <- vs !? n
        Just $ go restOfKey x
      (Nothing, v) -> v
      (Just (_, _), v) -> v

-- | Get the list of available keys inside a json value
listKeysInJSON :: ValueIR -> [Key]
listKeysInJSON = go True
  where
  go :: Bool -> ValueIR -> [Key]
  go isTopLevel value =
    case value of
      ObjectIR k (Just _) o ->
        (if isTopLevel then [] else [k]) ++
          concatMap (go False) (HashMap.elems o)
      ObjectIR _ _ o -> do
        concatMap (go False) (HashMap.elems o)
      ArrayIR _ vs -> do
        concatMap (go False) vs
      RawValue k _ _ ->
        if isTopLevel then [] else [k]
