{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.Extended 
  ( module Conferer.FromConfig
  , module GHC.Generics
  , configWith
  , anyConfigParserError
  , aConfigParserError
  , aMissingRequiredKey
  , aMissingRequiredKeys
  , aTypeMismatchWithDefaultError
  , (&)
  , ensureEmptyConfigThrows
  , ensureWrongTypeDefaultThrows
  , ensureUsingDefaultReturnsSameValue
  , ensureSingleConfigParsesTheRightThing
  , ensureSingleConfigThrowsParserError
  , ensureSingleConfigThrows
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
import Conferer.FromConfig.Internal
  ( MissingRequiredKey(..)
  , ConfigParsingError(..)
  )
import Conferer.FromConfig
import Conferer.Source.Simple (mkMapSource)

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addSource (mkMapSource keyValues)

anyConfigParserError :: ConfigParsingError -> Bool
anyConfigParserError _ = True

aConfigParserError :: Key -> Text -> ConfigParsingError -> Bool
aConfigParserError key txt (ConfigParsingError k t _) =
  key == k && t == txt

aMissingRequiredKey :: forall t. Typeable t => Key -> MissingRequiredKey -> Bool
aMissingRequiredKey key (MissingRequiredKey k t) =
  [key] == k && typeRep (Proxy :: Proxy t) == t

aMissingRequiredKeys :: forall t. Typeable t => [Key] -> MissingRequiredKey -> Bool
aMissingRequiredKeys keys (MissingRequiredKey k t) =
  keys == k && typeRep (Proxy :: Proxy t) == t

aTypeMismatchWithDefaultError :: forall a dyn. (Typeable dyn, Typeable a) =>
  Key -> dyn -> TypeMismatchWithDefault -> Bool
aTypeMismatchWithDefaultError key dyn e =
  e == typeMismatchWithDefault @a key (toDyn dyn)

data InvalidThing = InvalidThing deriving (Show, Eq)

ensureEmptyConfigThrows :: forall a. (Typeable a, FromConfig a) => SpecWith ()
ensureEmptyConfigThrows =
  context "with empty config"  $ do
    it "throws an exception" $ do
      config <- configWith []
      getFromConfig @a "some.key" config
        `shouldThrow` aMissingRequiredKey @a "some.key"

ensureWrongTypeDefaultThrows :: forall a. (Typeable a, FromConfig a) => SpecWith ()
ensureWrongTypeDefaultThrows =
  context "with invalid types in the defaults"  $ do
    it "throws an exception" $ do
      config <- configWith []
      getFromConfig @a "some.key" 
          (config & addDefault "some.key" InvalidThing)
        `shouldThrow` aTypeMismatchWithDefaultError @a "some.key" InvalidThing

ensureSingleConfigThrowsParserError ::
    forall a. (FromConfig a) => 
    Text -> SpecWith ()
ensureSingleConfigThrowsParserError keyContent =
  context "with invalid types in the defaults"  $ do
    it "throws an exception" $ do
      config <- configWith [("some.key", keyContent)]
      getFromConfig @a "some.key" config
        `shouldThrow` aConfigParserError "some.key" keyContent

ensureUsingDefaultReturnsSameValue ::
    forall a. (Typeable a, Eq a, Show a, FromConfig a) =>
    a -> SpecWith ()
ensureUsingDefaultReturnsSameValue value =
  context ("with a default of '" ++ show value ++ "'") $ do
    it "gets that same value" $ do
      config <- configWith []
      fetchedValue <- getFromConfig @a "some.key" 
        (config & addDefault "some.key" value)
      fetchedValue `shouldBe` value

ensureSingleConfigParsesTheRightThing :: 
    forall a. (Eq a, Show a, FromConfig a) =>
    Text -> a -> SpecWith ()
ensureSingleConfigParsesTheRightThing keyContent value =
  context ("with a config value of '" ++ unpack keyContent ++ "'" ) $ do
    it "gets the right value" $ do
      config <- configWith [("some.key", keyContent)]
      fetchedValue <- getFromConfig @a "some.key" config
      fetchedValue `shouldBe` value

ensureSingleConfigThrows :: 
    forall a e. (FromConfig a, Exception e) =>
    Text -> (e -> Bool) -> SpecWith ()
ensureSingleConfigThrows keyContent checkException =
  it "gets the right value" $ do
    config <- configWith [("some.key", keyContent)]
    getFromConfig @a "some.key" config
      `shouldThrow` checkException

ensureFetchParses ::
    forall a.
    ( FromConfig a
    , Eq a
    , Show a
    )
    => [(Key, Text)]
    -> [(Key, Dynamic)]
    -> a
    -> SpecWith ()
ensureFetchParses coso coso2 coso3 = it "gets the right value" $ do
  config <- withDefaults' (mapKeys coso2) <$> configWith (mapKeys coso)
  fetchedValue <- getFromConfig @a "some.key" config
  fetchedValue `shouldBe` coso3
  where
    mapKeys :: [(Key, any)] -> [(Key, any)]
    mapKeys = fmap (\(k, x) -> ("some.key" /. k, x))

ensureFetchThrows ::
    forall a e.
    ( FromConfig a
    , Exception e
    )
    => [(Key, Text)]
    -> [(Key, Dynamic)]
    -> (e -> Bool) 
    -> SpecWith ()
ensureFetchThrows coso coso2 coso3 = it "throws an exception" $ do
  config <- withDefaults' (mapKeys coso2) <$> configWith (mapKeys coso)
  getFromConfig @a "some.key" config
    `shouldThrow` coso3
  where
    mapKeys :: forall x. [(Key, x)] -> [(Key, x)]
    mapKeys = fmap (\(k, x) -> ("some.key" /. k, x))