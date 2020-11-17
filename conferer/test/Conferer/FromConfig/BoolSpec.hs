
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.BoolSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended

spec :: Spec
spec = do
  context "Basic fetching" $ do
    describe "fetching a Bool from config" $ do
      ensureEmptyConfigThrows @Bool
      ensureWrongTypeDefaultThrows @Bool

      ensureUsingDefaultReturnsSameValue @Bool True
      ensureUsingDefaultReturnsSameValue @Bool False

      ensureSingleConfigParsesTheRightThing @Bool "true" True
      ensureSingleConfigParsesTheRightThing @Bool "false" False

      context "parsing is case insensitive" $ do
        ensureSingleConfigParsesTheRightThing @Bool "faLSe" False
        ensureSingleConfigParsesTheRightThing @Bool "TRUE" True

      ensureSingleConfigThrowsParserError @Bool "nope"