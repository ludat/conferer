{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Conferer.Config.Internal where

import Data.Dynamic
import Data.List (sort, nub, union)
import Data.Text (Text)
import qualified Data.Map as Map

import Conferer.Key
import Conferer.Source.Internal
import Conferer.Config.Internal.Types
import Control.Monad (forM, msum)

type KeyMapping = (Key, Key)

data MappedKey = MappedKey 
  { mappingsChain :: [KeyMapping]
  , mappedKey :: Key
  } deriving (Show, Eq)

getKey :: Key -> Config -> IO KeyLookupResult
getKey key config = do
  let possibleKeys = getKeysFromMappings (configKeyMappings config) key
  untilJust (fmap (\MappedKey{..} -> getKeyInSources' mappedKey config) possibleKeys)
    >>= \case 
        Just (k, textResult) -> 
          return $ FoundInSources k textResult
        Nothing ->
          case msum $ fmap (\MappedKey{..} -> fmap (mappedKey,) $ (getKeyFromDefaults mappedKey config)) possibleKeys of
            Just (k, dynResult) -> return $ FoundInDefaults k dynResult
            Nothing -> return $ MissingKey [key]

listSubkeys :: Key -> Config -> IO [Key]
listSubkeys originalKey Config{..} = do
  let mappedKeys = getKeysFromMappings configKeyMappings originalKey
  subkeysFromSources <- forM mappedKeys $ \MappedKey{..} -> do
    subkeysFromSources <- go mappedKey [] configSources
    let subkeysFromDefaults =
          filter (mappedKey `isKeyPrefixOf`) $ Map.keys configDefaults
    return $ fmap (MappedKey mappingsChain) $ subkeysFromSources ++ subkeysFromDefaults
  let subkeys = mconcat subkeysFromSources
  return $ sort $ nub $ fmap undoMappings subkeys
  where
    go :: Key -> [Key] -> [Source] -> IO [Key]
    go _ result [] = return result
    go k result (source:otherSources) = do
      subkeys <- getSubkeysInSource source k
      go k (result `union` subkeys) otherSources

undoMappings :: MappedKey -> Key
undoMappings MappedKey{..} =
  go (reverse mappingsChain) mappedKey
  where
    go [] key = key
    go ((src, dest):others) key =
      case keyPrefixOf dest key of
        Just k -> go others (src /. k)
        Nothing -> go others key

untilJust :: [IO (Maybe a)] -> IO (Maybe a)
untilJust actions = go actions
  where
    go [] = return Nothing
    go (action:rest) = do
      action
        >>= \case 
          Just res -> return $ Just res
          Nothing -> go rest

getKeysFromMappings :: [KeyMapping] -> Key -> [MappedKey]
getKeysFromMappings originalKeyMappings originalKey =
  go (MappedKey [] originalKey) originalKeyMappings
  where
    go :: MappedKey -> [KeyMapping] -> [MappedKey]
    go k [] = [k]
    go currKey keyMappings =
      nub $
        currKey :
        mconcat (
        fmap generateDerivedKeys $
        findAndSplitList tryMappingKey
        keyMappings)
      where
        tryMappingKey :: (Key, Key) -> Maybe MappedKey
        tryMappingKey (source, dest) =
          case keyPrefixOf source (mappedKey currKey) of
            Just aKey ->
              Just $ MappedKey (mappingsChain currKey ++ [(source, dest)]) (dest /. aKey)
            Nothing -> Nothing 
        generateDerivedKeys :: ([KeyMapping], MappedKey, [KeyMapping]) -> [MappedKey]
        generateDerivedKeys (prevMappings, aKey, nextMappings) =
          go aKey $ prevMappings ++ nextMappings

findAndSplitList :: forall a b. (a -> Maybe b) -> [a] -> [([a], b, [a])]
findAndSplitList cond list = go [] list
  where
    go :: [a] -> [a] -> [([a], b, [a])]
    go _ [] = []
    go prevElems (curElem:nextElems) =
      case cond curElem of
        Just res -> 
          (prevElems, res, nextElems) : go (curElem:prevElems) nextElems
        Nothing -> 
          go (curElem:prevElems) nextElems

getKeyInSources' :: Key -> Config -> IO (Maybe (Key, Text))
getKeyInSources' k Config{..} =
  go configSources
  where
    go [] = return Nothing
    go (source:otherSources) = do
      res <- getKeyInSource source k
      case res of
        Just t -> return $ Just (k, t)
        Nothing -> go otherSources

getKeyFromDefaults :: Key -> Config -> Maybe Dynamic
getKeyFromDefaults key Config{..} =
  let 
    possibleKeys = fmap mappedKey $ getKeysFromMappings configKeyMappings key
  in msum $ fmap (\k -> Map.lookup k configDefaults) possibleKeys

-- | The empty configuration, this 'Config' is used as the base for
--   most config creating functions.
emptyConfig :: Config
emptyConfig = Config
  { configSources = []
  , configDefaults = Map.empty
  , configKeyMappings = []
  }

withKeyMappings :: [(Key, Key)] -> Config -> Config
withKeyMappings keyMappings config =
  config
  { configKeyMappings = keyMappings
  }

addKeyMappings :: [(Key, Key)] -> Config -> Config
addKeyMappings keyMappings config =
  config
  { configKeyMappings = configKeyMappings config ++ keyMappings
  }

withDefaults :: (Typeable a) => [(Key, a)] -> Config -> Config
withDefaults configMap config =
  config
  { configDefaults =
    (Map.map toDyn . Map.fromList $ configMap)
      `Map.union`
      (configDefaults config)
  }

withDefaults' :: [(Key, Dynamic)] -> Config -> Config
withDefaults' configMap config =
  config
  { configDefaults =
    (Map.fromList configMap)
      `Map.union`
      (configDefaults config)
  }

addDefault :: (Typeable a) => Key -> a -> Config -> Config
addDefault key value config =
  config
  { configDefaults = Map.insert key (toDyn value) $ configDefaults config
  }

removeDefault :: Key -> Config -> Config
removeDefault key config =
  config
  { configDefaults = Map.delete key $ configDefaults config
  }

-- | Instantiate a 'SourceCreator' using the 'emptyConfig'
mkStandaloneSource :: SourceCreator -> IO Source
mkStandaloneSource mkSource =
  mkSource emptyConfig

-- | Instantiate a 'Source' using an 'SourceCretor' and a 'Config' and add
--   to the config
addSource :: SourceCreator -> Config -> IO Config
addSource mkSource config = do
  newSource <- mkSource config
  return $
    config
    { configSources = configSources config ++ [ newSource ]
    }

-- orElse :: IO KeyLookupResult -> IO KeyLookupResult -> IO KeyLookupResult
-- orElse getKey1 getKey2 = do
--   result1 <- getKey1
--   case result1 of
--     MissingKey _ -> getKey2
--     FoundInSources _ _ -> return result1
--     FoundInDefaults _ _ -> do
--       result2 <- getKey2
--       case result2 of
--         MissingKey _ -> return result1
--         FoundInSources _ _ -> return result2
--         FoundInDefaults _ _ -> return result1