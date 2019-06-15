module Conferer.Provider.NamespacedSpec where

import Test.Hspec
import Conferer
import qualified Data.Map as Map

spec = do
  describe "namespaced config" $ do
    it "return nothing if the key doesn't match" $ do
      c <- emptyConfig
           & addProvider (mkNamespacedProvider "postgres"
                          $ mkMapConfigProvider [("url", "some url")])
      res <- getKey "url" c
      res `shouldBe` Left "Key 'url' was not found"
    it "returns the wrapped value" $ do
      c <- emptyConfig
           & addProvider (mkNamespacedProvider "postgres"
                          $ mkMapConfigProvider [("url", "some url")])
      res <- getKey "postgres.url" c
      res `shouldBe` Right "some url"
