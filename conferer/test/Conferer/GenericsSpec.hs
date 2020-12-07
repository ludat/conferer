{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Conferer.GenericsSpec where

import Test.Hspec

import Conferer.FromConfig
import Conferer.Config
import Conferer.Source.Simple

import GHC.Generics

data Thing = Thing
  { thingA :: Int
  , thingB :: Int
  } deriving (Generic, Show, Eq)

instance FromConfig Thing

instance DefaultConfig Thing where
  configDef = Thing 0 0

data Bigger = Bigger
  { biggerThing :: Thing
  , biggerB :: Int
  } deriving (Generic, Show, Eq)

instance FromConfig Bigger

instance DefaultConfig Bigger where
  configDef = Bigger (configDef {thingA = 27}) 1

spec :: Spec
spec = do
  describe "Generics" $ do
    context "with a simple record" $ do
      context "without a default but with all of the keys defined" $ do
        it "returns the default" $ do
          c <- emptyConfig
                & addSource (mkMapSource
                  [ ("somekey.a", "0")
                  , ("somekey.b", "0")
                  ])

          res <- fetchFromConfig @Thing "somekey" c
          res `shouldBe` Thing { thingA = 0, thingB = 0 }
      context "when no keys are set" $ do
        it "returns the default" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Thing)
                & addSource (mkMapSource [ ])

          res <- fetchFromConfig @Thing "somekey" c
          res `shouldBe` Thing { thingA = 0, thingB = 0 }
      context "when all keys are set" $ do
        it "return the keys set" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Thing)
                & addSource (mkMapSource
                  [ ("somekey.a", "1")
                  , ("somekey.b", "2")
                  ])

          res <- fetchFromConfig @Thing "somekey" c
          res `shouldBe` Thing { thingA = 1, thingB = 2 }

      context "when some keys are set" $ do
        it "uses the default and returns the keys set" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Thing)
                & addSource (mkMapSource
                  [ ("somekey.b", "2")
                  ])

          res <- fetchFromConfig @Thing "somekey" c
          res `shouldBe` Thing { thingA = 0, thingB = 2 }

    context "with a nested record" $ do
      context "when none of the keys are set" $ do
        it "returns the default of both records" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Bigger)
                & addSource (mkMapSource
                  [ ])

          res <- fetchFromConfig @Bigger "somekey" c
          res `shouldBe` Bigger { biggerThing = Thing { thingA = 27, thingB = 0 }, biggerB = 1}

      context "when some keys of the top record are set" $ do
        it "returns the default for the inner record" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Bigger)
                & addSource (mkMapSource
                  [ ("somekey.b", "30")
                  ])

          res <- fetchFromConfig @Bigger "somekey" c
          res `shouldBe` Bigger { biggerThing = Thing { thingA = 27, thingB = 0 }, biggerB = 30}

      context "when some keys of the inner record are set" $ do
        it "returns the inner record updated" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Bigger)
                & addSource (mkMapSource
                  [ ("somekey.thing.a", "30")
                  ])

          res <- fetchFromConfig @Bigger "somekey" c
          res `shouldBe` Bigger { biggerThing = Thing { thingA = 30, thingB = 0 }, biggerB = 1}

      context "when every key is set" $ do
        it "returns everything with the right values" $ do
          c <- emptyConfig
                & addDefault "somekey" (configDef @Bigger)
                & addSource (mkMapSource
                  [ ("somekey.thing.a", "10")
                  , ("somekey.thing.b", "20")
                  , ("somekey.b", "30")
                  ])

          res <- fetchFromConfig @Bigger "somekey" c
          res `shouldBe` Bigger { biggerThing = Thing { thingA = 10, thingB = 20 }, biggerB = 30}
