{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.StringLikeSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Conferer.FromConfig.Extended

spec :: Spec
spec = do
    describe "fetching a String from config" $ do
      ensureEmptyConfigThrows @String
      ensureWrongTypeDefaultThrows @String
      ensureUsingDefaultReturnsSameValue @String "aaa"
      ensureSingleConfigParsesTheRightThing @String "thing" "thing"
    describe "fetching text from config" $ do
      ensureEmptyConfigThrows @Text
      ensureWrongTypeDefaultThrows @Text
      ensureUsingDefaultReturnsSameValue @Text "aaa"
      ensureSingleConfigParsesTheRightThing @Text "thing" "thing"
