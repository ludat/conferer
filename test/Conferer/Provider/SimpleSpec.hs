module Conferer.Provider.SimpleSpec where


import Data.Aeson
import Test.Hspec

import Conferer

spec = do
  describe "json provider" $ do
    let creator =
          emptyConfig
          & addProvider (mkMapConfigProvider [ ("postgres.url", "some url")])

    it "getting a non existent key returns an empty config" $ do
      c <- creator
      res <- getKey "some.key" c
      res `shouldBe` Nothing

    it "getting an existent key returns unwraps the original map" $ do
      c <- creator
      res <- getKey "postgres.url" c
      res `shouldBe` Just "some url"
