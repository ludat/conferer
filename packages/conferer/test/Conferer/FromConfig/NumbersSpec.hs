
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.NumbersSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended

spec :: Spec
spec = do
  context "Numbers fetching" $ do
    describe "fetching an Int from config" $ do
      ensureEmptyConfigThrows @Int
      ensureUsingDefaultReturnsSameValue @Int 7
      ensureSingleConfigParsesTheRightThing @Int "7" 7
      ensureSingleConfigParsesTheRightThing @Int "-7" (-7)
      ensureSingleConfigThrowsParserError  @Int "50A"

    describe "fetching a Float from config" $ do
      ensureEmptyConfigThrows @Float
      ensureUsingDefaultReturnsSameValue @Float 7.5
      ensureSingleConfigParsesTheRightThing @Float "9.5" 9.5
      ensureSingleConfigParsesTheRightThing @Float "-9.5" (-9.5)
      ensureSingleConfigThrowsParserError @Float "9.50J"
      ensureSingleConfigThrowsParserError @Float ".5"
      ensureSingleConfigThrowsParserError @Float "9."
