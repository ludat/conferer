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
    it "getting an existing path returns the right value" $ do
      c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
      res <- getKeyInSource c "postgres.url"
      res `shouldBe` Just "some url"

    it "getting an non existing path returns nothing" $ do
      c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
      res <- getKeyInSource c "some.path"
      res `shouldBe` Nothing

    describe "with an array" $ do
      it "getting a path with number gets the right value" $ do
        c <- mk [aesonQQ| {"key": ["value"]} |]
        res <- getKeyInSource c "key.0"
        res `shouldBe` Just "value"

    describe "with an object" $ do
      it "getting an existing path returns nothing" $ do
        c <- mk [aesonQQ| {"key": { "path": "value"}} |]
        res <- getKeyInSource c "key"
        res `shouldBe` Nothing

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
    context "listing keys" $ do
      describe "listing keys with an empty object" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` []
      describe "gettings a existing subkey directly" $ do
        -- is this right? I'm not sure listing a keys should get the value :thinking:
        xit "gets no keys" $ do
          c <- mk [aesonQQ|{"key": 7}|]
          res <- getSubkeysInSource c "key"
          res `shouldBe` ["key"]
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
        it "gets no keys" $ do
          c <- mk [aesonQQ|{"a": {"a": 7}}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["a.a"]

