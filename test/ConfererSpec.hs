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
    let mkConfig =
          pure emptyConfig
          >>= addProvider (mkMapConfigProvider [ ("postgres.url", "some url")])
          >>= addProvider (mkMapConfigProvider [ ("postgres.url", "different url") , ("server.port", "4000")])

    it "getting a non existent key returns an empty config" $ do
      c <- mkConfig
      res <- getKey "aaa" c
      res `shouldBe` Left "Key 'aaa' was not found"

    it "getting an existent key returns unwraps the original map" $ do
      c <- mkConfig
      res <- getKey "postgres.url" c
      res `shouldBe` Right "some url"
    it "getting an existent key returns in the bottom maps gets it" $ do
      c <- mkConfig
      res <- getKey "server.port" c
      res `shouldBe` Right "4000"

