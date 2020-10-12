{-# LANGUAGE RecordWildCards #-}
module Conferer.Source.Namespaced
  (
    -- * Namespaced higher-order source
    -- | This source takes a source and returns a new source that
    -- always checks that the 'Key' given always starts with certain 'Key'
    -- and then strips that prefix before consulting its inner Source
    mkNamespacedSource
  ) where

import           Data.List (stripPrefix)

import           Conferer.Core
import           Conferer.Types

data NamespacedSource =
  NamespacedSource
  { scopeKey :: Key
  , innerSource :: Source
  } deriving (Show)

instance IsSource NamespacedSource where
  getKeyInSource (NamespacedSource {..}) (Path key) = do
    let Path scopeKeyPath = scopeKey
    case stripPrefix scopeKeyPath key of
      Just newKey -> getKeyInSource innerSource (Path newKey)
      Nothing -> return Nothing
  getSubkeysInSource (NamespacedSource {..}) (Path key) = do
    let Path scopeKeyPath = scopeKey
    case stripPrefix scopeKeyPath key of
      Just newKey -> do
        fmap (scopeKey /.) <$> getSubkeysInSource innerSource (Path newKey)
      Nothing -> return []


-- | Create a 'SourceCreator' from a prefix and another 'SourceCreator'
mkNamespacedSource :: Key -> SourceCreator -> SourceCreator
mkNamespacedSource scopeKey configCreator = \config -> do
  innerSource <- configCreator config
  return $
   Source $
   NamespacedSource
   {..}
