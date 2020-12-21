module Conferer.Source.NullSpec where

import           Test.Hspec

import           Conferer.Source
import           Conferer.Source.Null

spec :: Spec
spec = do
  it "always fails to get a key" $ do
    res <- getKeyInSource empty "some.key"
    res `shouldBe` Nothing
