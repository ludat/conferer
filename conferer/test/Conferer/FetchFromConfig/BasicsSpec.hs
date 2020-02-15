{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FetchFromConfig.BasicsSpec where

import           Test.Hspec
import           Conferer.Types
import           Data.Text
import           Conferer
import           Conferer.FetchFromConfig.Basics (fetch)
import           Control.Exception (evaluate)
import           Data.Typeable
import           Control.DeepSeq

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addProvider (mkMapProvider keyValues)

configParserError_ :: ConfigParsingError -> Bool
configParserError_ = const True

configParserError :: Key -> ConfigParsingError -> Bool
configParserError key (ConfigParsingError k _ _) =
  key == k

spec :: Spec
spec = context "Basics" $ do
  describe "fetching an Int from config" $ do
    it "getting a value that can't be parsed as an int returns an error message" $ do
      config <- configWith [ ("anInt", "50A") ]
      fetch @Int "anInt" config `shouldThrow` configParserError_
      -- evaluate (force fetchedValue) `shouldThrow` anyErrorCall

    it "getting a value that can be parsed correctly returns the int" $ do
      config <- configWith [ ("anInt", "50") ]
      fetchedValue <- fetch @Int "anInt" config
      fetchedValue `shouldBe` Just 50

  describe "fetching a Bool from config" $ do
    it "getting a value that can't be parsed as a bool returns an error message" $ do
      config <- configWith [ ("aBool", "nope") ]
      fetch @Bool "aBool" config `shouldThrow` configParserError_

    it "getting a value that can be parsed as a bool returns the bool" $ do
      config <- configWith [ ("aBool", "True"), ("anotherBool", "False") ]
      fetchedValue <- fetch "aBool" config
      fetchedValue `shouldBe` Just True
      anotherFetchedValue <- fetch "anotherBool" config
      anotherFetchedValue `shouldBe` Just False

    it "the parsing of the bool value is case insensitive" $ do
      config <- configWith [ ("aBool", "TRUE"), ("anotherBool", "fAlSe") ]
      fetchedValue <- fetch "aBool" config
      fetchedValue `shouldBe` Just True
      anotherFetchedValue <- fetch "anotherBool" config
      anotherFetchedValue `shouldBe` Just False

  describe "fetching a String from config" $ do
    it "getting a value returns the value as a string" $ do
      config <- configWith [ ("aString", "Bleh") ]
      fetchedValue <- fetch @String "aString" config
      fetchedValue `shouldBe` Just "Bleh"

  describe "fetching a Float from config" $ do
    it "if the value can be parsed as float, it returns that float" $ do
      config <- configWith [ ("aFloat", "9.5") ]
      fetchedValue <- fetch @Float "aFloat" config
      fetchedValue `shouldBe` Just 9.5

    it "if the value cannot be parsed as float, it fails" $ do
      config <- configWith [ ("aFloat", "ASD") ]
      fetch @Float "aFloat" config `shouldThrow` configParserError_

  describe "fetching a String from config" $ do
    context "when the key is there but has a wrong value" $ do
      it "returns Nothing" $ do
        config <- configWith [ ("anInt", "Bleh") ]
        fetch @Int "anInt" config `shouldThrow` configParserError_
    context "when the key is not there" $ do
      it "returns Nothing" $ do
        config <- configWith [ ]
        fetchedValue <- fetch @Int "anInt" config
        fetchedValue `shouldBe` Nothing
      it "returns Nothing" $ do
        config <- configWith [ ]
        fetchedValue <- fetch @(Maybe Int) "anInt" config
        fetchedValue `shouldBe` Just Nothing
