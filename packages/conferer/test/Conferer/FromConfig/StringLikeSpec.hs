{-# LANGUAGE TypeApplications #-}

module Conferer.FromConfig.StringLikeSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Conferer.FromConfig.Extended
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

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
    describe "fetching bytestring from config" $ do
      ensureEmptyConfigThrows @BS.ByteString
      ensureWrongTypeDefaultThrows @BS.ByteString
      ensureUsingDefaultReturnsSameValue @BS.ByteString "aaa"
      ensureSingleConfigParsesTheRightThing @BS.ByteString "thing" "thing"
    describe "fetching lazy bytestring from config" $ do
      ensureEmptyConfigThrows @LBS.ByteString
      ensureWrongTypeDefaultThrows @LBS.ByteString
      ensureUsingDefaultReturnsSameValue @LBS.ByteString "aaa"
      ensureSingleConfigParsesTheRightThing @LBS.ByteString "thing" "thing"
