{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfigSpec (spec) where

import Test.Hspec
import Conferer.FromConfig
import Conferer.FromConfig.Internal
import Conferer.FromConfig.Extended
import Conferer.Test

spec :: Spec
spec = do
  describe "Override FromConfig" $ do
    ensureFetchParses @Int
      []
      [ ("", overrideFetch @Int $ \_k _c -> pure 42) ]
      42

  describe "NotUserConfigurable" $ do
    describe "with a value of the inner type in the defaults" $ do
      ensureUsingDefaultReturnsSameValue @(NotUserConfigurable Int) $ NotUserConfigurable 42

    describe "with a value of the same type (with the wrapper)" $ do
      ensureFetchParses @(NotUserConfigurable Int)
        []
        [ ("", toDyn @(NotUserConfigurable Int) (NotUserConfigurable 42)) ]
        $ NotUserConfigurable 42

    describe "with both a value and a wrapped value, the wrapped values always takes priority" $ do
      ensureFetchParses @(NotUserConfigurable Int)
        []
        [ ("", toDyn @(NotUserConfigurable Int) $ NotUserConfigurable 1)
        , ("", toDyn @Int 2)
        ]
        $ NotUserConfigurable 1

    describe "with a fetch override of the inner type" $ do
      ensureFetchParses @(NotUserConfigurable Int)
        []
        [ ("", overrideFetch @Int $ \_k _c -> pure 42) ]
        $ NotUserConfigurable 42

    describe "with a fetch override of the same type (with the wrapper)" $ do
      ensureFetchParses @(NotUserConfigurable Int)
        []
        [ ("", overrideFetch @(NotUserConfigurable Int) $ \_k _c -> pure $ NotUserConfigurable 42) ]
        $ NotUserConfigurable 42
  describe "cosacosa" $ do
    context "with no explanations" $ do
      it "returns an empty exaplanation" $ do
        config <- configWith [] []
        cosacosa "" config
          `shouldBe` CompoundExplanation "" Nothing mempty
    context "with simple top level explanation" $ do
      it "returns that explanation" $ do
        config <- configWith [("", toDyn $ FetchExaplanation (\_ _ _ -> CompoundExplanation "Thing" Nothing mempty))] []
        cosacosa "" config
          `shouldBe` CompoundExplanation "Thing" Nothing mempty
    context "with simple nested explanation" $ do
      it "returns that explanation" $ do
        config <- configWith [("some.key", toDyn $ FetchExaplanation (\_ _ _ -> CompoundExplanation "Thing" Nothing mempty))] []
        cosacosa "some.key" config
          `shouldBe` CompoundExplanation "Thing" Nothing mempty
    context "with two configured explanations that overlap" $ do
      it "returns both explanations merged" $ do
        config <- configWith
          [ ("some.key", toDyn $ FetchExaplanation (\_ _ _ -> CompoundExplanation "Thing" Nothing mempty))
          , ("", toDyn $ FetchExaplanation (\_ _ _ -> CompoundExplanation "shouldn't" Nothing mempty))
          ]
          []
        cosacosa "some.key" config
          `shouldBe` CompoundExplanation "Thing" Nothing mempty


