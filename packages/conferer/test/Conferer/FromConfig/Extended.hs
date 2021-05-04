{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.Extended
  ( module Conferer.FromConfig
  , module GHC.Generics
  , configWith
  , ensureEmptyConfigThrows
  , ensureUsingDefaultReturnsSameValue
  , ensureSingleConfigParsesTheRightThing
  , ensureSingleConfigThrowsParserError
  , ensureSingleFetchThrows
  , ensureFetchParses
  , ensureFetchThrows
  , toDyn
  , module Conferer.Config
  ) where

import Data.Text
import Data.Typeable
import Test.Hspec
import GHC.Generics
import Control.Exception
import Data.Dynamic
import Data.Function

import Conferer.Config
import Conferer.FromConfig
import qualified Conferer.Source.InMemory as InMemory

configWith :: [(Key, Text)] -> IO Config
configWith keyValues =
  emptyConfig
  & addSource (InMemory.fromConfig keyValues)

ensureEmptyConfigThrows :: forall a. (HasCallStack, Typeable a, FromConfig a, Show a) => SpecWith ()
ensureEmptyConfigThrows =
  context "with empty config"  $ do
    it "throws an exception" $ do
      config <- configWith []
      fetchFromConfig @a "some.key" config
        `shouldThrowExactly` (missingRequiredKey @a "some.key" emptyConfig)

ensureSingleConfigThrowsParserError ::
    forall a. (HasCallStack, FromConfig a, Typeable a, Show a) =>
    Text -> SpecWith ()
ensureSingleConfigThrowsParserError keyContent =
  context "with invalid types in the defaults"  $ do
    it "throws an exception" $ do
      config <- configWith [("some.key", keyContent)]
      fetchFromConfig @a "some.key" config
        `shouldThrowExactly` (configParsingError @a "some.key" keyContent 0 emptyConfig)

shouldThrowExactly :: forall e a. (HasCallStack, Exception e, Eq e, Show a) => IO a -> e -> Expectation
shouldThrowExactly action expectedException = do
  result <- try @e action
  case result of
    Right a -> do
      fail $ "Expected an exception but got: " ++ show a
    Left e -> do
      e `shouldBe` expectedException

ensureUsingDefaultReturnsSameValue ::
    forall a. (HasCallStack, Typeable a, Eq a, Show a, FromConfig a) =>
    a -> SpecWith ()
ensureUsingDefaultReturnsSameValue value =
  context ("with a default of '" ++ show value ++ "'") $ do
    it "gets that same value" $ do
      config <- configWith []
      fetchedValue <- fetchFromConfig @a "some.key"
        (config & addDefault "some.key" value)
      fetchedValue `shouldBe` value

ensureSingleConfigParsesTheRightThing ::
    forall a. (HasCallStack, Eq a, Show a, FromConfig a, Typeable a) =>
    Text -> a -> SpecWith ()
ensureSingleConfigParsesTheRightThing keyContent value =
  context ("with a config value of '" ++ unpack keyContent ++ "'" ) $ do
    it "gets the right value" $ do
      config <- configWith [("some.key", keyContent)]
      fetchedValue <- fetchFromConfig @a "some.key" config
      fetchedValue `shouldBe` value

ensureSingleFetchThrows ::
    forall a e. (HasCallStack, FromConfig a, Typeable a, Exception e, Eq e, Show a) =>
    Text -> e -> SpecWith ()
ensureSingleFetchThrows keyContent checkException =
  it "gets the right value" $ do
    config <- configWith [("some.key", keyContent)]
    fetchFromConfig @a "some.key" config
      `shouldThrowExactly` checkException

ensureFetchParses ::
    forall a.
    ( HasCallStack
    , FromConfig a
    , Typeable a
    , Eq a
    , Show a
    )
    => [(Key, Text)]
    -> [(Key, Dynamic)]
    -> a
    -> SpecWith ()
ensureFetchParses configs defaults expectedValue =
  it "gets the right value" $ do
    config <- addDefaults (mapKeys defaults) <$> configWith (mapKeys configs)
    fetchedValue <- fetchFromConfig @a "some.key" config
    fetchedValue `shouldBe` expectedValue
  where
    mapKeys :: [(Key, any)] -> [(Key, any)]
    mapKeys = fmap (\(k, x) -> ("some.key" /. k, x))

ensureFetchThrows ::
    forall a e.
    ( FromConfig a
    , Typeable a
    , Exception e
    , Eq e
    , HasCallStack
    , Show a
    )
    => [(Key, Text)]
    -> [(Key, Dynamic)]
    -> e
    -> SpecWith ()
ensureFetchThrows configs defaults expectedException =
  it "throws an exception" $ do
    config <- addDefaults (mapKeys defaults) <$> configWith (mapKeys configs)
    fetchFromConfig @a "some.key" config
      `shouldThrowExactly` expectedException
  where
    mapKeys :: forall x. [(Key, x)] -> [(Key, x)]
    mapKeys = fmap (\(k, x) -> ("some.key" /. k, x))
