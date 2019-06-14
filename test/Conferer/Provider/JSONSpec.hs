module Conferer.Provider.JSONSpec where

import Data.Aeson.QQ.Simple
import Data.Aeson
import Test.Hspec

import Conferer

spec = do
  describe "json provider" $ do
    let c =
          mkJsonConfigProvider
          [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
    it "getting an existing path returns the right value" $ do
      res <- c `getKeyInProvider` "postgres.url"
      res `shouldBe` Just "some url"
    it "getting an non existing path returns nothing" $ do
      res <- c `getKeyInProvider` "some.path"
      res `shouldBe` Nothing

    describe "with an array" $ do
      let c =
            mkJsonConfigProvider
            [aesonQQ| {"key": ["value"]} |]
      it "getting a path with number gets the right value" $ do
        res <- c `getKeyInProvider` "key.0"
        res `shouldBe` Just "value"
      it "getting " True

    describe "with an object" $ do
      let c =
            mkJsonConfigProvider
            [aesonQQ| {"key": { "path": "value"}} |]
      it "getting an existing path returns nothing" $ do
        res <- c `getKeyInProvider` "key"
        res `shouldBe` Nothing

    describe "with a number" $ do
      let c =
            mkJsonConfigProvider
            [aesonQQ| {"key": 1.2} |]
      it "getting an existing path returns the right value" $ do
        res <- c `getKeyInProvider` "key"
        res `shouldBe` Just "1.2"

    describe "with a boolean" $ do
      let c =
            mkJsonConfigProvider
            [aesonQQ| {"key": false} |]
      it "getting an existing path returns the right value" $ do
        res <- c `getKeyInProvider` "key"
        res `shouldBe` Just "false"
