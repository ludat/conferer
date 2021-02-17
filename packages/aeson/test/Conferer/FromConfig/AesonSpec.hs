{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.AesonSpec where

import Data.Aeson
import Data.Aeson.QQ
import Test.Hspec

import Conferer.FromConfig
import Conferer.FromConfig.Aeson ()

import Conferer.Test

spec :: Spec
spec = do
  it "getting an existing path returns the right value" $ do
    c <- configWith [] [("some.key", "{\"some\": \"some url\", \"ssl\": true}")]
    res <- fetchFromConfig @Value "some.key" c
    res `shouldBe` [aesonQQ| {"some": "some url", "ssl": true} |]
