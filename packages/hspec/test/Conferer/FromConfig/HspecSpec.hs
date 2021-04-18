{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.HspecSpec where

import Test.Hspec
import qualified Test.Hspec.Core.Runner as Hspec

import Conferer
import Conferer.Test
import Conferer.FromConfig.Hspec ()

spec :: Spec
spec = do
  describe "with an empty config" $ do
    it "throws an exception" $ do
      config <- configWith [] []
      unsafeFetchKey @Hspec.Config config "warp"
        `shouldThrow` anyException
  describe "a config that has a default" $ do
    it "doesn't throw" $ do
      config <- configWith [] []
      _ <- fetchKey @Hspec.Config config "hspec" configDef
      return ()
