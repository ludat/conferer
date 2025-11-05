-- |
-- Copyright: (c) 2025 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Documentation support for configuration keys
module Conferer.Config.Docs
  ( ConfigDocs(..)
  , KeyDoc(..)
  , emptyDocs
  , doc
  , docWithType
  , docWithExample
  , docWithDefault
  , docFull
  , addKeyDoc
  , addKeyTypeDoc
  , addKeyExampleDoc
  , addKeyDefaultDoc
  , explainKey
  , generateSchema
  , getConfigDocs
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, mapMaybe)
import Conferer.Key (Key)
import qualified Conferer.Key as Key
import Conferer.Config.Internal.Types (Config(..), KeyLookupResult(..))
import Conferer.Config.Internal (getKeyFromSources, getKeyFromDefaults)
import Data.Typeable (Typeable, typeRep, Proxy(..))
import Data.Dynamic (Dynamic, dynTypeRep)

-- | Documentation for a configuration key
data KeyDoc = KeyDoc
  { keyDocDescription :: Maybe Text
    -- ^ Human-readable description of what this key does
  , keyDocType :: Maybe Text
    -- ^ Expected type of the value
  , keyDocExample :: Maybe Text
    -- ^ Example value
  , keyDocDefault :: Maybe Text
    -- ^ Default value if any
  } deriving (Show, Eq)

-- | Container for all configuration documentation
newtype ConfigDocs = ConfigDocs
  { unConfigDocs :: Map Key KeyDoc
  } deriving (Show, Eq)

-- | Empty documentation
instance Semigroup ConfigDocs where
  ConfigDocs a <> ConfigDocs b = ConfigDocs (Map.union a b)

instance Monoid ConfigDocs where
  mempty = ConfigDocs Map.empty

-- | Create empty documentation
emptyDocs :: ConfigDocs
emptyDocs = mempty

-- | Add documentation for a key (simple description only)
doc :: Key -> Text -> ConfigDocs -> ConfigDocs
doc = addKeyDoc

-- | Add documentation with type information
docWithType :: Key -> Text -> Text -> ConfigDocs -> ConfigDocs
docWithType key description typeName docs =
  addKeyTypeDoc key typeName $ addKeyDoc key description docs

-- | Add documentation with example
docWithExample :: Key -> Text -> Text -> ConfigDocs -> ConfigDocs
docWithExample key description example docs =
  addKeyExampleDoc key example $ addKeyDoc key description docs

-- | Add documentation with default value
docWithDefault :: Key -> Text -> Text -> ConfigDocs -> ConfigDocs
docWithDefault key description defaultVal docs =
  addKeyDefaultDoc key defaultVal $ addKeyDoc key description docs

-- | Add full documentation for a key
docFull
  :: Key
  -> Text  -- ^ Description
  -> Text  -- ^ Type
  -> Text  -- ^ Default value
  -> Text  -- ^ Example
  -> ConfigDocs
  -> ConfigDocs
docFull key description typeName defaultVal example docs =
  ConfigDocs $ Map.insert key fullDoc (unConfigDocs docs)
  where
    fullDoc = KeyDoc
      (Just description)
      (Just typeName)
      (Just example)
      (Just defaultVal)

-- | Add documentation for a key
addKeyDoc
  :: Key
  -> Text  -- ^ Description
  -> ConfigDocs
  -> ConfigDocs
addKeyDoc key description (ConfigDocs docs) =
  ConfigDocs $ Map.insertWith mergeDoc key newDoc docs
  where
    newDoc = KeyDoc (Just description) Nothing Nothing Nothing
    mergeDoc new old = old
      { keyDocDescription = keyDocDescription new <> keyDocDescription old
      }

-- | Add type information to a key's documentation
addKeyTypeDoc
  :: Key
  -> Text  -- ^ Type name
  -> ConfigDocs
  -> ConfigDocs
addKeyTypeDoc key typeName (ConfigDocs docs) =
  ConfigDocs $ Map.insertWith mergeDoc key newDoc docs
  where
    newDoc = KeyDoc Nothing (Just typeName) Nothing Nothing
    mergeDoc new old = old { keyDocType = keyDocType new <> keyDocType old }

-- | Add example to a key's documentation
addKeyExampleDoc
  :: Key
  -> Text  -- ^ Example value
  -> ConfigDocs
  -> ConfigDocs
addKeyExampleDoc key example (ConfigDocs docs) =
  ConfigDocs $ Map.insertWith mergeDoc key newDoc docs
  where
    newDoc = KeyDoc Nothing Nothing (Just example) Nothing
    mergeDoc new old = old { keyDocExample = keyDocExample new <> keyDocExample old }

-- | Add default value to a key's documentation
addKeyDefaultDoc
  :: Key
  -> Text  -- ^ Default value
  -> ConfigDocs
  -> ConfigDocs
addKeyDefaultDoc key defaultVal (ConfigDocs docs) =
  ConfigDocs $ Map.insertWith mergeDoc key newDoc docs
  where
    newDoc = KeyDoc Nothing Nothing Nothing (Just defaultVal)
    mergeDoc new old = old { keyDocDefault = keyDocDefault new <> keyDocDefault old }

-- | Get all documentation
getConfigDocs :: ConfigDocs -> Map Key KeyDoc
getConfigDocs = unConfigDocs

-- | Explain a specific key (like kubectl explain)
-- Returns detailed information about a configuration key including:
-- - Documentation
-- - Current value and source
-- - Type information
-- - Default value
explainKey :: Config -> ConfigDocs -> Key -> IO Text
explainKey config (ConfigDocs docs) key = do
  -- Look up the key in sources
  let sourceResult = getKeyFromSources key config

  -- Look up in defaults (we need a type, so we'll check for any Dynamic)
  let defaultsMap = configDefaults config
      defaultValues = Map.lookup key defaultsMap

  -- Get documentation
  let keyDoc = Map.lookup key docs

  -- Build explanation
  let header = "Key: " <> Key.keyName key <> "\n"
              <> Text.replicate (Text.length (Key.keyName key) + 5) "=" <> "\n\n"

      descSection = case keyDoc >>= keyDocDescription of
        Just desc -> "Description:\n  " <> desc <> "\n\n"
        Nothing -> "Description:\n  (No documentation available)\n\n"

      typeSection = case keyDoc >>= keyDocType of
        Just typeName -> "Type:\n  " <> typeName <> "\n\n"
        Nothing -> case defaultValues of
          Just (d:_) -> "Type:\n  " <> (Text.pack . show . dynTypeRep $ d) <> "\n\n"
          _ -> ""

      currentSection = case sourceResult of
        FoundInSources value sourceIdx _ ->
          "Current Value:\n  " <> value <> "\n  Source: sources["
          <> Text.pack (show sourceIdx) <> "]\n\n"
        _ -> case defaultValues of
          Just (_:_) -> "Current Value:\n  (from default)\n\n"
          _ -> "Current Value:\n  (not set)\n\n"

      defaultSection = case keyDoc >>= keyDocDefault of
        Just defVal -> "Default:\n  " <> defVal <> "\n\n"
        Nothing -> ""

      exampleSection = case keyDoc >>= keyDocExample of
        Just ex -> "Example:\n  " <> ex <> "\n\n"
        Nothing -> ""

  return $ header <> descSection <> typeSection <> currentSection
          <> defaultSection <> exampleSection

-- | Generate a schema/help document for all configured keys
generateSchema :: ConfigDocs -> Text
generateSchema (ConfigDocs docs) =
  let title = "Configuration Schema\n"
             <> "====================\n\n"

      entries = Map.toList docs

      formatEntry :: (Key, KeyDoc) -> Text
      formatEntry (key, KeyDoc{..}) =
        let keyHeader = "### " <> Key.keyName key <> "\n\n"
            desc = fromMaybe "(No description)" keyDocDescription <> "\n\n"
            typeInfo = case keyDocType of
              Just t -> "- **Type:** `" <> t <> "`\n"
              Nothing -> ""
            defaultInfo = case keyDocDefault of
              Just d -> "- **Default:** `" <> d <> "`\n"
              Nothing -> ""
            exampleInfo = case keyDocExample of
              Just ex -> "- **Example:** `" <> ex <> "`\n"
              Nothing -> ""
        in keyHeader <> desc <> typeInfo <> defaultInfo <> exampleInfo <> "\n"

  in if Map.null docs
     then "No configuration documentation available.\n"
     else title <> Text.concat (map formatEntry entries)
