{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE TypeApplications #-}

module ConfererSpec where

import Test.Hspec

import Data.Text (Text)
import Conferer.FromConfig (fetchFromConfig)
import Conferer.Source.InMemory (fromConfig)
import Data.Dynamic (toDyn)
import Conferer.Config (Config)
import Conferer (mkConfig', Key)

spec :: Spec
spec = do
  describe "mkConfig'" $ do
    let a = fromConfig [("keyPresentInBoth", "valueInA"), ("keyPresentOnlyInA", "valueInA")]
        b = fromConfig [("keyPresentInBoth", "valueInB"), ("keyPresentOnlyInB", "valueInB")]
        shouldHaveKeyWithValue :: Config -> Key -> Text -> Expectation
        shouldHaveKeyWithValue config key expected = do
          actual <- fetchFromConfig key config
          actual `shouldBe` expected
    describe "the precedence of the sources is defined by the order in the list" $ do
      it "if several sources have a value for the key, the first source in the list that has it is used" $ do
        config <- mkConfig' [] [a, b]

        config `shouldHaveKeyWithValue` "keyPresentInBoth" $ "valueInA"
      it "if only one source has a value for the key, that source is used" $ do
        config <- mkConfig' [] [a, b]

        config `shouldHaveKeyWithValue` "keyPresentOnlyInB" $ "valueInB"
      it "if the key is not present in any source, it doesn't retrieve any value" $ do
        config <- mkConfig' [] [a, b]

        value <- fetchFromConfig "keyNotPresentInAnySource" config
        value `shouldBe` (Nothing :: Maybe Text)
    describe "adds the defaults to the config" $ do
      it "if the key is present in a source and in the default, it uses the first source that has it" $ do
        config <- mkConfig' [("keyPresentOnlyInA", toDyn @Text "defaultValue")] [a]

        config `shouldHaveKeyWithValue` "keyPresentOnlyInA" $ "valueInA"
      it "if the key is not present in any source but it's in a default, it uses the default" $ do
        config <- mkConfig' [("keyPresentOnlyInA", toDyn @Text "defaultValue")] [b]

        config `shouldHaveKeyWithValue` "keyPresentOnlyInA" $ "defaultValue"
