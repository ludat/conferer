module Conferer.Source.MappingSpec where

import           Test.Hspec
import qualified Data.Map as Map

import           Conferer

spec :: Spec
spec = do
  describe "with a mapping source" $ do
    it "getting an existent key in the original map but that's not mapped in the \
       \wrapper doesn't exist" $ do
      c <- emptyConfig
           & addSource (mkMappingSource Map.empty
                          $ mkMapSource [("some.key", "some value")])
      res <- getKey "some.key" c
      res `shouldBe` Nothing

    it "getting a non existent key isn't there" $ do
      c <- emptyConfig
           & addSource (mkMappingSource (Map.fromList [("k", "key")])
                          $ mkMapSource [("key", "75")])

      res <- getKey "xxxx" c
      res `shouldBe` Nothing

    it "getting an existent key that's mapped but doesn't exist on the \
       \inner source isn't there" $ do
      c <- emptyConfig
           & addSource (mkMappingSource (Map.fromList [("another.key", "some.key")])
                          $ mkMapSource [])

      res <- getKey "another.key" c
      res `shouldBe` Nothing

    it "getting an existent key that's mapped properly gets it and exists on \
       \the inner source gets it" $ do
      c <- emptyConfig
           & addSource (mkMappingSource (Map.fromList [("another.key", "some.key")])
                          $ mkMapSource [("some.key", "some value")])
      res <- getKey "another.key" c
      res `shouldBe` Just "some value"
