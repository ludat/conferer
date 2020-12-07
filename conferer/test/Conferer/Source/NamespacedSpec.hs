module Conferer.Source.NamespacedSpec where

import Test.Hspec

import Conferer.Source
import Conferer.Config.Internal

import Conferer.Source.Simple
import Conferer.Source.Namespaced

spec :: Spec
spec = do
  describe "namespaced config" $ do
    it "return nothing if the key doesn't match" $ do
      c <- mkStandaloneSource $
        mkNamespacedSource "postgres" $
          mkMapSource [("url", "some url")]
      res <- getKeyInSource c "url"
      res `shouldBe` Nothing
    it "returns the wrapped value" $ do
      c <- mkStandaloneSource $
        mkNamespacedSource "postgres" $
          mkMapSource [("url", "some url")]
      res <- getKeyInSource c "postgres.url"
      res `shouldBe` Just "some url"
    it "listing subkeys" $ do
      c <- mkStandaloneSource $
        mkNamespacedSource "postgres" $
          mkMapSource [("url", "some url")]
      res <- getSubkeysInSource c "postgres"
      res `shouldBe` ["postgres.url"]

