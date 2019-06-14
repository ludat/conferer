module Conferer.Provider.SimpleSpec where


import Data.Aeson
import Test.Hspec

import Conferer

spec = do
  describe "json provider" $ do
    let p =
          mkMapConfigProvider
            [ ("postgres.url", "some url")
            ]

    it "getting a non existent key returns an empty config" $ do
      res <- p `getKeyInProvider` "some.key"
      res `shouldBe` Nothing

    it "getting an existent key returns unwraps the original map" $ do
      res <- p `getKeyInProvider` "postgres.url"
      res `shouldBe` Just "some url"
