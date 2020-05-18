module Conferer.Source.NullSpec where

import           Test.Hspec

import           Conferer

spec :: Spec
spec = do
  it "always fails to get a key" $ do
    c <- emptyConfig
         & addSource mkNullSource
    res <- getKey "some.key" c
    res `shouldBe` Nothing
