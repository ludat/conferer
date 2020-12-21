module Conferer.KeySpec where

import Test.Hspec

import Conferer.Key
import Conferer.Key.Internal

spec :: Spec
spec = do
  describe "keys" $ do
    it "parsing keys does the right thing" $ do
      "some.key" `shouldBe` Path ["some", "key"]
    it "an empty string is the empty list" $ do
      "" `shouldBe` Path []
    it "are matched in a case insensitive way" $ do
      "some.key" `shouldBe` ("SoME.KeY" :: Key)
    it "'s true representation is case insensitive" $ do
      "somE.Key" `shouldBe` Path ["some", "key"]
