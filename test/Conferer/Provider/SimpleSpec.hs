module Conferer.Provider.SimpleSpec where


import Data.Aeson
import Test.Hspec

import Conferer

spec = do
  describe "json provider" $ do
    let creator =
          emptyConfig
          & addProvider (mkMapProvider [ ("postgres.url", "some url")])

    it "getting a non existent key returns an empty config" $ do
      c <- creator
      res <- getKey "some.key" c
      res `shouldBe` Left "Key 'some.key' was not found"

    it "getting an existent key returns unwraps the original map" $ do
      c <- creator
      res <- getKey "postgres.url" c
      res `shouldBe` Right "some url"
