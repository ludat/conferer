module Conferer.Source.JSONSpec where

import Data.Aeson.QQ
import Test.Hspec

import Conferer.Source
import Conferer.Config.Internal (mkStandaloneSource)
import Conferer.Source.JSON

spec :: Spec
spec = do
  describe "json source" $ do
    let mk value =
          mkStandaloneSource $
            mkJsonSource' value
    describe "#getKeyInSource" $ do
      it "getting an existing path returns the right value" $ do
        c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
        res <- getKeyInSource c "postgres.url"
        res `shouldBe` Just "some url"

      it "getting an non existing path returns nothing" $ do
        c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
        res <- getKeyInSource c "some.path"
        res `shouldBe` Nothing

      it "getting an non existing path returns nothing" $ do
        c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
        res <- getKeyInSource c "some.path"
        res `shouldBe` Nothing

      describe "with an array" $ do
        it "getting a path with number gets the right value" $ do
          c <- mk [aesonQQ| {"key": ["value"]} |]
          res <- getKeyInSource c "key.0"
          res `shouldBe` Just "value"
        describe "magic 'keys' key" $ do
          context "with an empy array" $ do
            it "the magic 'keys' key is \"\"" $ do
              c <- mk [aesonQQ| {"key": []} |]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just ""
          context "with a non empty array" $ do
            it "the magic 'keys' key is a comma separated list of all \
               \indexes present on the array" $ do
              c <- mk [aesonQQ| {"key": [true, true, true]}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just "0,1,2"

      describe "with an object" $ do
        it "getting an existing path returns nothing" $ do
          c <- mk [aesonQQ| {"key": { "path": "value"}}|]
          res <- getKeyInSource c "key"
          res `shouldBe` Nothing
        describe "magic 'keys' key" $ do
          context "with an empy object" $ do
            it "the magic 'keys' key is \"\"" $ do
              c <- mk [aesonQQ| {"key": {}}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just ""
          context "with a non empty object" $ do
            it "the magic 'keys' key is a comma separated list of all \
               \keys present in the object" $ do
              c <- mk [aesonQQ| {"key": {a: true, b: true, c: true}}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just "a,b,c"
          context "with an object that has a 'keys' key present" $ do
            it "return its value instead of doing the metamagic" $ do
              c <- mk [aesonQQ| {"key": {keys: "something", c: true}}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just "something"

      describe "with an int" $ do
        it "getting an existing path returns the right value" $ do
          c <- mk [aesonQQ| {"key": 1} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "1"

      describe "with a float" $ do
        it "getting an existing path returns the right value" $ do
          c <- mk [aesonQQ| {"key": 1.2} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "1.2"

      describe "with a boolean" $ do
        it "getting an existing path returns the right value" $ do
          c <- mk [aesonQQ| {"key": false} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "false"


    context "#getSubkeysInSource" $ do
      describe "listing keys in non existant path" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{}|]
          res <- getSubkeysInSource c "a"
          res `shouldBe` []
      describe "listing keys with an empty object" $ do
        it "gets only the 'keys' magic key" $ do
          c <- mk [aesonQQ|{}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` []
      describe "gettings a existing subkey directly" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{"key": 7}|]
          res <- getSubkeysInSource c "key"
          res `shouldBe` []
      describe "getting the subkeys for an object" $ do
        it "gets the keys" $ do
          c <- mk [aesonQQ|{"key": 7}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["key"]
      describe "gettings the keys from a list and an object" $ do
        it "gets the keys as index numbers" $ do
          c <- mk [aesonQQ|{"a": ["a"]}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["a.0"]
      describe "getting the keys from subkeys" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{"a": ["a"]}|]
          res <- getSubkeysInSource c "a"
          res `shouldBe` ["a.0"]
      describe "gettings the keys from nested objects" $ do
        it "gets those keys and all intermediate magic 'keys' keys" $ do
          c <- mk [aesonQQ|{"a": {"a": 7}}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["a.a"]
      describe "gettings the keys from list" $ do
        it "returns the present indexes" $ do
          c <- mk [aesonQQ|[true, true, true]|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["0", "1", "2"]

