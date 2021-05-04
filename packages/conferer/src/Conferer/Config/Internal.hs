-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Internal module providing Config functionality
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Conferer.Config.Internal where

import Control.Monad (foldM, forM, msum)
import Data.Dynamic
import Data.List (sort, nub, union)
import Data.Text (Text)
import Data.Maybe (isJust)
import qualified Data.Map as Map

import Conferer.Key
import Conferer.Source.Internal
import Conferer.Config.Internal.Types
#if __GLASGOW_HASKELL__ < 808
import Data.Void (absurd)
#endif

-- | This function runs lookups on the 'Config', first in 'Source's in order and
-- then inside the defaults.
--
-- NOTE: that this function is parameterized on the type of the result, which means that type must
-- match exactly for a successful lookup on the defaults.
getKey :: forall a. Typeable a => Key -> Config -> IO (KeyLookupResult ('BothSourcesAndDefaults a))
getKey key config = do
  getKeyFromSources key config
    >>= \case
        FoundInSources textResult sourceIndex k ->
          return $ FoundInSources textResult sourceIndex k
        MissingKey () triedKeysForSources ->
          case getKeyFromDefaults @a key config of
            MissingKey () _triedKeysForDefaults ->
              return $ MissingKey () triedKeysForSources
            FoundInDefaults v k ->
              return $ FoundInDefaults v k
#if __GLASGOW_HASKELL__ < 808
            FoundInSources v _ _ -> absurd v
        FoundInDefaults v _ -> absurd v
#endif
-- | This function looks for a 'Key' inside the 'Source's of a 'Config'
--
-- This is utility function that has proved to be useful but when doubt
-- use 'getKey'
getKeyFromSources :: Key -> Config -> IO (KeyLookupResult ('OnlySources))
getKeyFromSources key config = do
  let possibleKeys = getKeysFromMappings (configKeyMappings config) key
  untilJust (fmap (\MappedKey{..} -> getRawKeyInSources mappedKey config) possibleKeys)
    >>= \case
      Just (k, i, t) -> pure $ FoundInSources t i k
      Nothing -> pure $ MissingKey () $ fmap mappedKey possibleKeys

-- | Alias for a mapping from one key to another used for transforming keys
type KeyMapping = (Key, Key)

-- | A key that has been transformed using one or many 'KeyMapping's, so that
--   that process can be reversed.
data MappedKey = MappedKey
  { mappingsChain :: [KeyMapping]
  , mappedKey :: Key
  } deriving (Show, Eq)


-- | This function lists all available keys under some key, that could be fetched
-- successfully from the 'Source's.
listSubkeys :: Key -> Config -> IO [Key]
listSubkeys originalKey Config{..} = do
  let mappedKeys = getKeysFromMappings configKeyMappings originalKey
  subkeysFromSources <- forM mappedKeys $ \MappedKey{..} -> do
    subkeysFromSources <- listRawSubkeysInSources mappedKey configSources
    return $ fmap (MappedKey mappingsChain) subkeysFromSources
  let subkeys = mconcat subkeysFromSources
  return $ sort $ nub $ fmap undoMappings subkeys
  where
    listRawSubkeysInSources :: Key -> [Source] -> IO [Key]
    listRawSubkeysInSources mappedKey = go mappedKey []
      where
        go :: Key -> [Key] -> [Source] -> IO [Key]
        go _ result [] = return result
        go k result (source:otherSources) = do
          subkeys <- getSubkeysInSource source k
          go k (result `union` subkeys) otherSources

-- | This function reverses the mappings in a 'MappedKey' to retrieve the
--   original key.
--
--   Assumes that mappings were really used, otherwise it ignores bad values
undoMappings :: MappedKey -> Key
undoMappings MappedKey{..} =
  go (reverse mappingsChain) mappedKey
  where
    go [] key = key
    go ((src, dest):others) key =
      case stripKeyPrefix dest key of
        Just k -> go others (src /. k)
        Nothing -> go others key

-- | This utility function run a list of IO actions and returns the
--   first that return a 'Just', if no one does, returns 'Nothing'
untilJust :: [IO (Maybe a)] -> IO (Maybe a)
untilJust actions = go actions
  where
    go [] = return Nothing
    go (action:rest) = do
      action
        >>= \case
          Just res -> return $ Just res
          Nothing -> go rest

-- | This function tries to apply a list of mappings to a key meaning
-- replace the prefix with the new value from the mapping, if the mapping
-- isn't a prefix that mapping is ignored
--
-- This function always terminates even in presence of recursive mappings,
-- since it removes the mapping after it was first used, and that causes that
-- eventually the function will run out of keymappings and terminate.
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
          case stripKeyPrefix source (mappedKey currKey) of
            Just aKey ->
              Just $ MappedKey (mappingsChain currKey ++ [(source, dest)]) (dest /. aKey)
            Nothing -> Nothing
        generateDerivedKeys :: ([KeyMapping], MappedKey, [KeyMapping]) -> [MappedKey]
        generateDerivedKeys (prevMappings, aKey, nextMappings) =
          go aKey $ prevMappings ++ nextMappings

-- | This utility function splits a list based on a @cond@ function and returns a tuple
--   of previous value, next values and the mapped found value.
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

-- | This function gets a value from 'Source's but ignores mappings and defaults
getRawKeyInSources :: Key -> Config -> IO (Maybe (Key, Int, Text))
getRawKeyInSources k Config{..} =
  go $ zip [0..] configSources
  where
    go [] = return Nothing
    go ((index, source):otherSources) = do
      res <- getKeyInSource source k
      case res of
        Just t -> return $ Just (k, index, t)
        Nothing -> go otherSources

-- | Fetch from value from the defaults map of a 'Config' or else return a 'Nothing'
getKeyFromDefaults :: forall a. (Typeable a) => Key -> Config -> KeyLookupResult ('OnlyDefaultsAs a)
getKeyFromDefaults key Config{..} =
  let
    possibleKeys = fmap mappedKey $ getKeysFromMappings configKeyMappings key
  in case msum . fmap fetchKey $ possibleKeys of
      Just (k, v) -> FoundInDefaults v k
      Nothing -> MissingKey () possibleKeys
  where
    fromDynamics :: forall d. (Typeable d) => [Dynamic] -> Maybe d
    fromDynamics =
      msum . fmap (fromDynamic @d)

    fetchKey :: Key -> Maybe (Key, a)
    fetchKey k = do
      dynamics <- Map.lookup k configDefaults
      value <- fromDynamics @a dynamics
      pure (k, value)

-- | The empty configuration, this 'Config' is used as the base for
--   most config creating functions.
emptyConfig :: Config
emptyConfig = Config
  { configSources = []
  , configDefaults = Map.empty
  , configKeyMappings = []
  }

-- | This function adds some key mappings to a 'Config'
addKeyMappings :: [KeyMapping] -> Config -> Config
addKeyMappings keyMappings config =
  config
  { configKeyMappings = configKeyMappings config ++ keyMappings
  }

-- | This function adds defaults to a 'Config'
addDefaults :: [(Key, Dynamic)] -> Config -> Config
addDefaults configMap config =
  let
    constructedMap =
      Map.fromListWith (++)
      . fmap (\(k,v) -> (k, [v]))
      $ configMap
  in config
  { configDefaults =
    Map.unionWith (++) constructedMap
      $ configDefaults config
  }

-- | This function removes a default from a 'Config', this is the
-- oposite of 'addDefault', it deletes the first element of
-- matching type in a certain 'Key'.
removeDefault :: forall t. Typeable t => Key -> Config -> Config
removeDefault key config =
  config
  { configDefaults =
      Map.update removeFirstDynamic key $ configDefaults config
  }
  where
    removeFirst :: (a -> Bool) -> [a] -> [a]
    removeFirst _ [] = []
    removeFirst condition (x:xs) =
      if condition x
        then xs
        else x : removeFirst condition xs

    removeFirstDynamic :: [Dynamic] -> Maybe [Dynamic]
    removeFirstDynamic dynamics =
      let result = removeFirst (isJust . fromDynamic @t) dynamics
      in if null result
          then Nothing
          else Just result

-- | This function adds one default of a custom type to a 'Config'
--
--   Note that unlike 'addDefaults' this function does the toDyn so
--   no need to do it on the user's side
addDefault :: (Typeable a) => Key -> a -> Config -> Config
addDefault key value config =
  config
  { configDefaults = Map.insertWith (++) key [toDyn value] $ configDefaults config
  }

-- | Instantiate a 'Source' using an 'SourceCreator' and a 'Config' and add
--   to the config
addSource :: SourceCreator -> Config -> IO Config
addSource mkSource config = do
  newSource <- mkSource config
  return $
    config
    { configSources = configSources config ++ [ newSource ]
    }

-- | Instantiate several 'Source's using a 'SourceCreator's and a 'Config' and add
--   them to the config in the order defined by the list
addSources :: [SourceCreator] -> Config -> IO Config
addSources sources config = foldM (flip addSource) config sources

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
