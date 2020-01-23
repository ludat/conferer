module Conferer.Provider.JSONSpec where

import Data.Aeson.QQ
import Test.Hspec

import Conferer
import Conferer.Provider.JSON

spec :: Spec
spec = do
  describe "json provider" $ do
    it "getting an existing path returns the right value" $ do
      c <- emptyConfig
           & addProvider (mkJsonProvider' [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |])
      res <- getKey "postgres.url" c
      res `shouldBe` Just "some url"

    it "getting an non existing path returns nothing" $ do
      c <- emptyConfig
           & addProvider (mkJsonProvider' [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |])
      res <- getKey "some.path" c
      res `shouldBe` Nothing

    describe "with an array" $ do
      it "getting a path with number gets the right value" $ do
        c <- emptyConfig
             & addProvider (mkJsonProvider'
                          [aesonQQ| {"key": ["value"]} |])
        res <- getKey "key.0" c
        res `shouldBe` Just "value"

    describe "with an object" $ do
      it "getting an existing path returns nothing" $ do
        c <- emptyConfig
           & addProvider (mkJsonProvider'
            [aesonQQ| {"key": { "path": "value"}} |])
        res <- getKey "key" c
        res `shouldBe` Nothing

    describe "with an int" $ do
      it "getting an existing path returns the right value" $ do
        c <- emptyConfig
           & addProvider (mkJsonProvider' [aesonQQ| {"key": 1} |])
        res <- getKey "key" c
        res `shouldBe` Just "1"

    describe "with a float" $ do
      it "getting an existing path returns the right value" $ do
        c <- emptyConfig
           & addProvider (mkJsonProvider' [aesonQQ| {"key": 1.2} |])
        res <- getKey "key" c
        res `shouldBe` Just "1.2"

    describe "with a boolean" $ do
      it "getting an existing path returns the right value" $ do
        c <- emptyConfig
           & addProvider (mkJsonProvider' [aesonQQ| {"key": false} |])
        res <- getKey "key" c
        res `shouldBe` Just "false"
