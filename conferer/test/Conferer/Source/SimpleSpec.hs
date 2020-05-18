module Conferer.Source.SimpleSpec where

import Test.Hspec

import Conferer

spec :: Spec
spec = do
  describe "json source" $ do
    let creator =
          emptyConfig
          & addSource (mkMapSource [ ("postgres.url", "some url")])

    it "getting a non existent key returns an empty config" $ do
      c <- creator
      res <- getKey "some.key" c
      res `shouldBe` Nothing

    it "getting an existent key returns unwraps the original map" $ do
      c <- creator
      res <- getKey "postgres.url" c
      res `shouldBe` Just "some url"
