module Conferer.FetchFromConfig.BasicsSpec where

import           Test.Hspec
import           Conferer.Types
import           Data.Text
import           Conferer
import           Conferer.FetchFromConfig.Basics ()

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addProvider (mkMapProvider keyValues)

spec :: Spec
spec = do
  describe "fetching an Int from config" $ do
    it "getting a value that can't be parsed as an int returns an error message" $ do
      config <- configWith [ ("anInt", "50A") ]
      fetchedValue <- fetch "anInt" config
      fetchedValue `shouldBe` (Left "Key anInt could not be parsed correctly" :: Either Text Int)
    it "getting a value that can be parsed correctly returns the int" $ do
      config <- configWith [ ("anInt", "50") ]
      fetchedValue <- fetch "anInt" config
      fetchedValue `shouldBe` (Right 50 :: Either Text Int)

  describe "fetching a Bool from config" $ do
    it "getting a value that can't be parsed as a bool returns an error message" $ do
      config <- configWith [ ("aBool", "nope") ]
      fetchedValue <- fetch "aBool" config
      fetchedValue `shouldBe` (Left "Key aBool could not be parsed correctly" :: Either Text Bool)
    it "getting a value that can be parsed as a bool returns the bool" $ do
      config <- configWith [ ("aBool", "True"), ("anotherBool", "False") ]
      fetchedValue <- fetch "aBool" config
      fetchedValue `shouldBe` (Right True :: Either Text Bool)
      anotherFetchedValue <- fetch "anotherBool" config
      anotherFetchedValue `shouldBe` (Right False :: Either Text Bool)
    it "the parsing of the bool value is case insensitive" $ do
      config <- configWith [ ("aBool", "TRUE"), ("anotherBool", "fAlSe") ]
      fetchedValue <- fetch "aBool" config
      fetchedValue `shouldBe` (Right True :: Either Text Bool)
      anotherFetchedValue <- fetch "anotherBool" config
      anotherFetchedValue `shouldBe` (Right False :: Either Text Bool)

  describe "fetching a String from config" $ do
    it "getting a value returns the value as a string" $ do
      config <- configWith [ ("aString", "Bleh") ]
      fetchedValue <- fetch "aString" config
      fetchedValue `shouldBe` (Right "Bleh" :: Either Text String)

  describe "fetching a Float from config" $ do
    it "if the value can be parsed as float, it returns that float" $ do
      config <- configWith [ ("aFloat", "9.5") ]
      fetchedValue <- fetch "aFloat" config
      fetchedValue `shouldBe` (Right 9.5 :: Either Text Float)
    it "if the value cannot be parsed as float, it fails" $ do
      config <- configWith [ ("aFloat", "ASD") ]
      fetchedValue <- fetch "aFloat" config
      fetchedValue `shouldBe` (Left "Key aFloat could not be parsed correctly" :: Either Text Float)

  describe "fetching a Maybe from config" $ do
    it "getting a value returns the value as a Just string" $ do
      config <- configWith [ ("aString", "Bleh") ]
      fetchedValue <- fetch "aString" config
      fetchedValue `shouldBe` (Right (Just "Bleh") :: Either Text (Maybe String))
    context "with an empty value that's present" $ do
      it "returns Nothing" $ do
        config <- configWith [ ("aString", "") ]
        fetchedValue <- fetch "aString" config
        fetchedValue `shouldBe` (Right Nothing :: Either Text (Maybe String))
