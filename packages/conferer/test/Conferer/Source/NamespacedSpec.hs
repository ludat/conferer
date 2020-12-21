module Conferer.Source.NamespacedSpec where

import Test.Hspec

import Conferer.Source

import qualified Conferer.Source.InMemory as InMemory
import Conferer.Source.Namespaced

spec :: Spec
spec = do
  describe "namespaced config" $ do
    let source =
          fromInner "postgres" $
            InMemory.fromAssociations [("url", "some url")]
    it "return nothing if the key doesn't match" $ do
      res <- getKeyInSource source "url"
      res `shouldBe` Nothing
    it "returns the wrapped value" $ do
      res <- getKeyInSource source "postgres.url"
      res `shouldBe` Just "some url"
    it "listing subkeys" $ do
      res <- getSubkeysInSource source "postgres"
      res `shouldBe` ["postgres.url"]

