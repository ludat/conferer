module Conferer.Source.MappingSpec where

import           Test.Hspec
import qualified Data.Map as Map

import           Conferer

spec :: Spec
spec = do
  describe "with a mapping source" $ do
    let mk mappingMap innerMap =
           mkStandaloneSource $ mkMappingSource (Map.fromList mappingMap) $ mkMapSource innerMap
    it "getting an existent key in the original map but that's not mapped in the \
       \wrapper doesn't exist" $ do
      c <- mk [] [("some.key", "some value")]
      res <- getKeyInSource c "some.key"
      res `shouldBe` Nothing

    it "getting a non existent key isn't there" $ do
      c <- mk [("k", "key")] [("key", "75")]

      res <- getKeyInSource c "xxxx"
      res `shouldBe` Nothing

    it "getting an existent key that's mapped but doesn't exist on the \
       \inner source isn't there" $ do
      c <- mk [("another.key", "some.key")] []

      res <- getKeyInSource c "another.key"
      res `shouldBe` Nothing

    it "getting an existent key that's mapped properly gets it and exists on \
       \the inner source gets it" $ do
      c <- mk [("another.key", "some.key")] [("some.key", "some value")]
      res <- getKeyInSource c "another.key"
      res `shouldBe` Just "some value"

    it "dsa" $ do
      c <- mk [] [("some.key", "some value")]
      res <- getSubkeysInSource c "another.key"
      res `shouldBe` []
    it "getting" $ do
      -- TODO check this because it's kinda weird
      c <- mk [("another.key", "some.key"), ("another", "some")] [("some.key", "some value")]
      res <- getSubkeysInSource c "another"
      res `shouldBe` ["another.key"]