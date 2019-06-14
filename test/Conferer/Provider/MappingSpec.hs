module Conferer.Provider.MappingSpec where

import           Test.Hspec
import qualified Data.Map as Map
import           Data.Function ((&))

import           Conferer

spec = do
  describe "with a mapping provider" $ do
    let originalProvider =
          mkMapConfigProvider [("some.key", "some value")]

    it "getting an existent key in the original map but that's not mapped in the \
       \wrapper doesn't exist" $ do
      let p = originalProvider
              & mkMappingProvider Map.empty
      res <- p `getKeyInProvider` "some.key"
      res `shouldBe` Nothing

    it "getting a non existent key isn't there" $ do
      let p = originalProvider
              & mkMappingProvider Map.empty
      res <- p `getKeyInProvider` "xxxx"
      res `shouldBe` Nothing

    it "getting an existent key that's mapped but doesn't exist on the \
       \inner provider isn't there" $ do
      let p = mkMapConfigProvider []
              & mkMappingProvider
              (Map.fromList
                [ ("another.key", "some.key")
                ])
      res <- p `getKeyInProvider` "another.key"
      res `shouldBe` Nothing

    it "getting an existent key that's mapped properly gets it and exists on \
       \the inner provider gets it" $ do
      let p = originalProvider
              & mkMappingProvider
              (Map.fromList
               [ ("another.key", "some.key")
               ])
      res <- p `getKeyInProvider` "another.key"
      res `shouldBe` Just "some value"
