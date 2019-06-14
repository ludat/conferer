module Conferer.Provider.NamespacedSpec where

import Test.Hspec
import Conferer


spec = do
  describe "namespaced config" $ do
    let c =
          mkNamespacedProvider "postgres"
          $ mkMapConfigProvider
          [ ("url", "some url")
          ]
    it "return nothing if the key doesn't match" $ do
      res <- c `getKeyInProvider` "url"
      res `shouldBe` Nothing
    it "returns the wrapped value" $ do
      res <- c `getKeyInProvider` "postgres.url"
      res `shouldBe` Just "some url"
