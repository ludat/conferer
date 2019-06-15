module Conferer.Provider.JSONSpec where

import Data.Aeson.QQ.Simple
import Data.Aeson
import Test.Hspec

import Conferer

spec = do
  describe "json provider" $ do
    it "getting an existing path returns the right value" $ do
      c <- emptyConfig
           & addProvider (mkJsonConfigProvider [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |])
      res <- getKey "postgres.url" c
      res `shouldBe` Just "some url"
    it "getting an non existing path returns nothing" $ do
      c <- emptyConfig
           & addProvider (mkJsonConfigProvider [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |])
      res <- getKey "some.path" c
      res `shouldBe` Nothing

    describe "with an array" $ do
      it "getting a path with number gets the right value" $ do
        c <- emptyConfig
             & addProvider (mkJsonConfigProvider
                          [aesonQQ| {"key": ["value"]} |])
        res <- getKey "key.0" c
        res `shouldBe` Just "value"
      it "getting " True

    describe "with an object" $ do
      it "getting an existing path returns nothing" $ do
        c <- emptyConfig
           & addProvider (mkJsonConfigProvider
            [aesonQQ| {"key": { "path": "value"}} |])
        res <- getKey "key" c
        res `shouldBe` Nothing

    describe "with a number" $ do
      let c =
            mkJsonConfigProvider
      it "getting an existing path returns the right value" $ do
        c <- emptyConfig
           & addProvider (mkJsonConfigProvider [aesonQQ| {"key": 1.2} |])
        res <- getKey "key" c
        res `shouldBe` Just "1.2"

    describe "with a boolean" $ do
      let c =
            mkJsonConfigProvider
      it "getting an existing path returns the right value" $ do
        c <- emptyConfig
           & addProvider (mkJsonConfigProvider [aesonQQ| {"key": false} |])
        res <- getKey "key" c
        res `shouldBe` Just "false"
