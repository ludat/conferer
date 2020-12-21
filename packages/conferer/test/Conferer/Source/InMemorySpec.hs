module Conferer.Source.InMemorySpec where

import Test.Hspec

import Conferer.Source
import Conferer.Source.InMemory

spec :: Spec
spec = do
  describe "in memory source" $ do
    let source =
          fromAssociations [("postgres.url", "some url")]

    it "getting a non existent key returns an empty config" $ do
      res <- getKeyInSource source "some.key"
      res `shouldBe` Nothing

    it "getting an existent key returns unwraps the original map" $ do
      res <- getKeyInSource source "postgres.url"
      res `shouldBe` Just "some url"

    it "listing subkeys works" $ do
      res <- getSubkeysInSource source "postgres"
      res `shouldBe` ["postgres.url"]

    it "listing subkeys never returns the same key" $ do
      res <- getSubkeysInSource source "postgres.url"
      res `shouldBe` []

    it "listing subkeys that are not present" $ do
      res <- getSubkeysInSource source "stuff"
      res `shouldBe` []
