{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Conferer.GenericsSpec where

import Test.Hspec

import Conferer.FromConfig

import GHC.Generics
import Conferer.FromConfig.Extended

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

data WithUnderscoreAndConstructor = WithUnderscoreAndConstructor
  { _withUnderscoreAndConstructor_number :: Int
  , _withUnderscoreAndConstructor_name :: String
  } deriving (Generic, Show, Eq)

instance FromConfig WithUnderscoreAndConstructor

instance DefaultConfig WithUnderscoreAndConstructor where
  configDef = WithUnderscoreAndConstructor 0 ""

data WithUnderscoreOnly = WithUnderscoreOnly
  { _number :: Int
  , _name :: String
  } deriving (Generic, Show, Eq)

instance FromConfig WithUnderscoreOnly

instance DefaultConfig WithUnderscoreOnly where
  configDef = WithUnderscoreOnly 0 ""

spec :: Spec
spec = do
  describe "Generics" $ do
    context "with a simple record" $ do
      context "without a default but with all of the keys defined" $ do
        ensureFetchParses
          [ ("a", "0")
          , ("b", "0")
          ]
          []
          Thing { thingA = 0, thingB = 0 }
      context "when no keys are set" $ do
        ensureFetchParses
          []
          [ ("", toDyn $ configDef @Thing)
          ]
          Thing { thingA = 0, thingB = 0 }
      context "when all keys are set" $ do
        ensureFetchParses
          [ ("a", "1")
          , ("b", "2")
          ]
          [ ("", toDyn $ configDef @Thing)
          ]
          Thing { thingA = 1, thingB = 2 }

      context "when some keys are set" $ do
        ensureFetchParses
          [ ("b", "2")
          ]
          [ ("", toDyn $ configDef @Thing)
          ]
          Thing { thingA = 0, thingB = 2 }

    context "with a nested record" $ do
      context "when none of the keys are set" $ do
        ensureFetchParses
          [ ]
          [ ("", toDyn @Bigger configDef )
          ]
          Bigger { biggerThing = Thing { thingA = 27, thingB = 0 }, biggerB = 1}

      context "when some keys of the top record are set" $ do
        ensureFetchParses
          [ ("b", "30")
          ]
          [ ("", toDyn @Bigger configDef)
          ]
          Bigger { biggerThing = Thing { thingA = 27, thingB = 0 }, biggerB = 30}

      context "when some keys of the inner record are set" $ do
        ensureFetchParses
          [ ("thing.a", "30")
          ]
          [ ("", toDyn @Bigger configDef)
          ]
          Bigger { biggerThing = Thing { thingA = 30, thingB = 0 }, biggerB = 1}

      context "when every key is set" $ do
        ensureFetchParses
          [ ("thing.a", "10")
          , ("thing.b", "20")
          , ("b", "30")
          ]
          [ ("", toDyn @Bigger configDef)
          ]
          Bigger { biggerThing = Thing { thingA = 10, thingB = 20 }, biggerB = 30}

    context "with a record that has fields with underscore and constructor" $ do
      context "without a default but with all of the keys defined" $ do
        ensureFetchParses
          [ ("number", "7")
          , ("name", "juan")
          ]
          []
          WithUnderscoreAndConstructor { _withUnderscoreAndConstructor_name = "juan", _withUnderscoreAndConstructor_number = 7 }
      context "when no keys are set" $ do
        ensureFetchParses
          []
          [ ("", toDyn $ configDef @WithUnderscoreAndConstructor)
          ]
          WithUnderscoreAndConstructor { _withUnderscoreAndConstructor_name = "", _withUnderscoreAndConstructor_number = 0 }
      context "when all keys are set" $ do
        ensureFetchParses
          [ ("number", "1")
          , ("name", "ludat")
          ]
          [ ("", toDyn $ configDef @WithUnderscoreAndConstructor)
          ]
          WithUnderscoreAndConstructor { _withUnderscoreAndConstructor_name = "ludat", _withUnderscoreAndConstructor_number = 1 }

      context "when some keys are set" $ do
        ensureFetchParses
          [ ("number", "2")
          ]
          [ ("", toDyn $ configDef @WithUnderscoreAndConstructor)
          ]
          WithUnderscoreAndConstructor { _withUnderscoreAndConstructor_name = "", _withUnderscoreAndConstructor_number = 2 }

    context "with a record that has fields with leading underscores" $ do
      context "without a default but with all of the keys defined" $ do
        ensureFetchParses
          [ ("number", "7")
          , ("name", "juan")
          ]
          []
          WithUnderscoreOnly { _name = "juan", _number = 7 }
      context "when no keys are set" $ do
        ensureFetchParses
          []
          [ ("", toDyn $ configDef @WithUnderscoreOnly)
          ]
          WithUnderscoreOnly { _name = "", _number = 0 }
      context "when all keys are set" $ do
        ensureFetchParses
          [ ("number", "1")
          , ("name", "ludat")
          ]
          [ ("", toDyn $ configDef @WithUnderscoreOnly)
          ]
          WithUnderscoreOnly { _name = "ludat", _number = 1 }

      context "when some keys are set" $ do
        ensureFetchParses
          [ ("number", "2")
          ]
          [ ("", toDyn $ configDef @WithUnderscoreOnly)
          ]
          WithUnderscoreOnly { _name = "", _number = 2 }