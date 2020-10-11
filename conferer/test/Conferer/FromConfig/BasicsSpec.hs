{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.BasicsSpec where

import Conferer
import Conferer.Types
import Data.Text
import Data.Typeable
import Test.Hspec

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addSource (mkMapSource keyValues)

configParserError_ :: ConfigParsingError -> Bool
configParserError_ = const True

configParserError :: Key -> Text -> ConfigParsingError -> Bool
configParserError key txt (ConfigParsingError k t _) =
  key == k && t == txt

requiredKeyError :: Key -> TypeRep -> MissingRequiredKey -> Bool
requiredKeyError key type_ (MissingRequiredKey k t) =
  key == k && type_ == t

spec :: Spec
spec = do
  context "Basic fetching" $ do
    -- describe "" $ do
    --   it "cosa" $ do
    --     config <- configWith
    --       [ ("aList.0", "50")
    --       ]
    --     fetchedValue <- getFromConfig @([Int]) "aList" config
    --     fetchedValue `shouldBe` [50]
    describe "fetching an Int from config" $ do
      it "getting a value that can't be parsed as an int returns an error message" $ do
        config <- configWith [ ("anInt", "50A") ]
        getFromConfig @(Maybe Int) "anInt" config `shouldThrow` configParserError "anInt" "50A"

      it "getting a value that can be parsed correctly returns the int" $ do
        config <- configWith [ ("anInt", "50") ]
        fetchedValue <- getFromConfig @(Maybe Int) "anInt" config
        fetchedValue `shouldBe` Just 50

    describe "fetching a Bool from config" $ do
      it "getting a value that can't be parsed as a bool returns an error message" $ do
        config <- configWith [ ("aBool", "nope") ]
        getFromConfig @(Maybe Bool) "aBool" config `shouldThrow` configParserError_

      it "getting a value that can be parsed as a bool returns the bool" $ do
        config <- configWith [ ("aBool", "True"), ("anotherBool", "False") ]
        fetchedValue <- getFromConfig "aBool" config
        fetchedValue `shouldBe` Just True
        anotherFetchedValue <- getFromConfig "anotherBool" config
        anotherFetchedValue `shouldBe` Just False

      it "the parsing of the bool value is case insensitive" $ do
        config <- configWith [ ("aBool", "TRUE"), ("anotherBool", "fAlSe") ]
        fetchedValue <- getFromConfig "aBool" config
        fetchedValue `shouldBe` Just True
        anotherFetchedValue <- getFromConfig "anotherBool" config
        anotherFetchedValue `shouldBe` Just False

    describe "fetching a String from config" $ do
      it "getting a value returns the value as a string" $ do
        config <- configWith [ ("aString", "Bleh") ]
        fetchedValue <- getFromConfig @(Maybe String) "aString" config
        fetchedValue `shouldBe` Just "Bleh"

    describe "fetching a Float from config" $ do
      it "if the value can be parsed as float, it returns that float" $ do
        config <- configWith [ ("aFloat", "9.5") ]
        fetchedValue <- getFromConfig @(Maybe Float) "aFloat" config
        fetchedValue `shouldBe` Just 9.5

      it "if the value cannot be parsed as float, it fails" $ do
        config <- configWith [ ("aFloat", "ASD") ]
        getFromConfig @(Maybe Float) "aFloat" config `shouldThrow` configParserError_

    describe "fetching a String from config" $ do
      context "when the key is there but has a wrong value" $ do
        it "returns Nothing" $ do
          config <- configWith [ ("anInt", "Bleh") ]
          getFromConfig @(Maybe Int) "anInt" config `shouldThrow` configParserError_
      context "when the key is not there" $ do
        it "returns Nothing" $ do
          config <- configWith [ ]
          fetchedValue <- getFromConfig @(Maybe Int) "anInt" config
          fetchedValue `shouldBe` Nothing
        it "returns Nothing" $ do
          config <- configWith [ ]
          fetchedValue <- getFromConfig @(Maybe (Maybe Int)) "anInt" config
          fetchedValue `shouldBe` Just Nothing
  context "Required values" $ do
    describe "when getting a value that's required" $ do
      it "returns an error with fetching information like key and type" $ do
        config <- configWith [ ]
        getFromConfigWithDefault @Int "anInt" config requiredValue
          `shouldThrow` requiredKeyError "anInt" (typeRep (Proxy :: Proxy Int))
