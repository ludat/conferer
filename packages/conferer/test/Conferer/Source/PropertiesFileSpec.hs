module Conferer.Source.PropertiesFileSpec where

import Test.Hspec

import Conferer.Source
import Conferer.Source.PropertiesFile

spec :: Spec
spec = do
  describe "with a properties file config" $ do
    let mk = return . fromFileContent "file.properties"
    describe "#getKeyInSource" $ do
      it "getting an existent key returns unwraps top level value (without \
         \children)" $ do
        c <- mk "some.key=a value"
        res <- getKeyInSource c "some.key"
        res `shouldBe` Just "a value"

      it "getting an existent key for a child gets that value" $ do
        c <- mk "some.key=b"
        res <- getKeyInSource c "some.key"
        res `shouldBe` Just "b"

      it "keys should always be consistent as to how the words are separated" $ do
        c <- mk "some.key=a value"
        res <- getKeyInSource c "some.key"
        res `shouldBe` Just "a value"

      it "respects leading spaces in values" $ do
        c <- mk "some.key= a value "
        res <- getKeyInSource c "some.key"
        res `shouldBe` Just " a value "
    describe "#explainNotFound" $ do
      it "recommends adding the right line to the file" $ do
        s <- mk ""
        explainNotFound s "some.key"
          `shouldBe` "Adding a new line 'some.key=some value' to the file 'file.properties'"
    describe "#explainSettedKey" $ do
      it "recommends adding the right line to the file" $ do
        s <- mk "some.key=thing"
        explainSettedKey s "some.key"
          `shouldBe` "key 'some.key' (on file 'file.properties')"

