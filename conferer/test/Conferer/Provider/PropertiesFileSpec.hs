module Conferer.Provider.PropertiesFileSpec where

import           Test.Hspec
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

import Conferer
import Conferer.Core
import Conferer.Provider.PropertiesFile

fakeLookupEnv :: [(String, String)] -> LookupEnvFunc
fakeLookupEnv fakeEnv = \envName ->
  return $ Map.lookup envName $ Map.fromList fakeEnv

spec :: Spec
spec = do
  describe "with a properties file config" $ do
    it "getting an existent key returns unwraps top level value (wihtout \
       \children)" $ do
      c <- mkStandaloneProvider $ mkPropertiesFileProvider' "some.key=a value"
      res <- getKeyInProvider c "some.key"
      res `shouldBe` Just "a value"

    it "getting an existent key for a child gets that value" $ do
      c <- mkStandaloneProvider $ mkPropertiesFileProvider' $ Text.unlines
                               [ "some.key=a value"
                               , "some.key=b"
                               ]
      res <- getKeyInProvider c "some.key"
      res `shouldBe` Just "b"

    it "keys should always be consistent as to how the words are separated" $ do
      c <- mkStandaloneProvider $ mkPropertiesFileProvider' $ Text.unlines
                               [ "some.key=a value"
                               , "some.key=b"
                               ]
      res <- getKeyInProvider c "another.key"
      res `shouldBe` Nothing
