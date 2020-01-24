module Conferer.Provider.NamespacedSpec where

import Test.Hspec

import Conferer

spec :: Spec
spec = do
  describe "namespaced config" $ do
    it "return nothing if the key doesn't match" $ do
      c <- emptyConfig
           & addProvider (mkNamespacedProvider "postgres"
                          $ mkMapProvider [("url", "some url")])
      res <- getKey "url" c
      res `shouldBe` Nothing
    it "returns the wrapped value" $ do
      c <- emptyConfig
           & addProvider (mkNamespacedProvider "postgres"
                          $ mkMapProvider [("url", "some url")])
      res <- getKey "postgres.url" c
      res `shouldBe` Just "some url"
