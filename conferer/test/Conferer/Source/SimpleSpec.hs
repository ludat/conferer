module Conferer.Source.SimpleSpec where

import Test.Hspec

import Conferer.Source
import Conferer.Config.Internal
import Conferer.Source.Simple

spec :: Spec
spec = do
  describe "json source" $ do
    let creator = mkStandaloneSource (mkMapSource [("postgres.url", "some url")])

    it "getting a non existent key returns an empty config" $ do
      c <- creator
      res <- getKeyInSource c "some.key"
      res `shouldBe` Nothing

    it "getting an existent key returns unwraps the original map" $ do
      c <- creator
      res <- getKeyInSource c "postgres.url"
      res `shouldBe` Just "some url"

    it "listing subkeys works" $ do
      c <- creator
      res <- getSubkeysInSource c "postgres"
      res `shouldBe` ["postgres.url"]

    it "listing subkeys works" $ do
      c <- creator
      res <- getSubkeysInSource c "postgres.url"
      res `shouldBe` ["postgres.url"]

    it "listing subkeys that are not present" $ do
      c <- creator
      res <- getSubkeysInSource c "stuff"
      res `shouldBe` []
