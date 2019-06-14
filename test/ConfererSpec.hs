module ConfererSpec where

import Data.Aeson
import Data.Aeson.QQ.Simple
import Test.Hspec
import Conferer

spec = do
  describe "keys" $ do
    it "parsing keys does the right thing" $ do
      "some.key" `shouldBe` Path ["some", "key"]
    it "an empty string is the empty list" $ do
      "" `shouldBe` Path []

  describe "with a config with a nested value" $ do
    let c =
          Config
          [ mkMapConfigProvider
            [ ("postgres.url", "some url")
            ]
          , mkMapConfigProvider
            [ ("postgres.url", "different url")
            , ("server.port", "4000")
            ]
          ]

    it "getting a non existent key returns an empty config" $ do
      res <- getKey "aaa" c
      res `shouldBe` Nothing

    it "getting an existent key returns unwraps the original map" $ do
      res <- getKey "postgres.url" c
      res `shouldBe` Just "some url"

