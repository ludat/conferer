module Conferer.FetchFromConfig.BasicsSpec where

import           Test.Hspec
import           Conferer.Types
import           Data.Text
import           Conferer
import           Conferer.FetchFromConfig.Basics

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addProvider (mkMapConfigProvider keyValues)

spec = do
  describe "fetching an Int from config" $ do
    it "getting a non existant key returns an error message" $ do
      config <- configWith [ ("anInt", "500") ]
      fetchedValue <- fetch "nonExistingKey" config
      fetchedValue `shouldBe` (Left "Key nonExistingKey was not found" :: Either Text Int)
    it "getting an existant key that can't be parsed as an int returns an error message" $ do
      config <- configWith [ ("anInt", "50A") ]
      fetchedValue <- fetch "anInt" config
      fetchedValue `shouldBe` (Left "Key anInt could not be parsed correctly" :: Either Text Int)
    it "getting an existant key that can be parsed correctly returns the int" $ do
      config <- configWith [ ("anInt", "50") ]
      fetchedValue <- fetch "anInt" config
      fetchedValue `shouldBe` (Right 50 :: Either Text Int)