module Conferer.Provider.MappingSpec where

import           Test.Hspec
import qualified Data.Map as Map
import           Data.Function ((&))

import           Conferer

spec = do
  describe "with a mapping provider" $ do
    let innerProvider =
          mkMapConfigProvider
          [ ("some.key", "some value")
          ]

    it "getting an existent key in the original map but that's not mapped in the \
       \wrapper doesn't exist" $ do
      c <- emptyConfig
           & addProvider (mkMappingProvider Map.empty
                          $ mkMapConfigProvider [("some.key", "some value")])
      res <- getKey "some.key" c
      res `shouldBe` Left "Key 'some.key' was not found"

    it "getting a non existent key isn't there" $ do
      c <- emptyConfig
           & addProvider (mkMappingProvider (Map.fromList [("k", "key")])
                          $ mkMapConfigProvider [("key", "75")])

      res <- getKey "xxxx" c
      res `shouldBe` Left "Key 'xxxx' was not found"

    it "getting an existent key that's mapped but doesn't exist on the \
       \inner provider isn't there" $ do
      c <- emptyConfig
           & addProvider (mkMappingProvider (Map.fromList [("another.key", "some.key")])
                          $ mkMapConfigProvider [])

      res <- getKey "another.key" c
      res `shouldBe` Left "Key 'another.key' was not found"

    it "getting an existent key that's mapped properly gets it and exists on \
       \the inner provider gets it" $ do
      c <- emptyConfig
           & addProvider (mkMappingProvider (Map.fromList [("another.key", "some.key")])
                          $ mkMapConfigProvider [("some.key", "some value")])
      res <- getKey "another.key" c
      res `shouldBe` Right "some value"
