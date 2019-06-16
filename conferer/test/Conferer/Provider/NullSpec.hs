module Conferer.Provider.NullSpec where

import           Test.Hspec

import           Conferer

spec = do
  it "always fails to get a key" $ do
    c <- emptyConfig
         & addProvider mkNullProvider
    res <- getKey "some.key" c
    res `shouldBe` Left "Key 'some.key' was not found"
