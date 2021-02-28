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
