{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfigSpec (spec) where

import Test.Hspec
import Conferer.FromConfig
import Conferer.FromConfig.Extended

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

