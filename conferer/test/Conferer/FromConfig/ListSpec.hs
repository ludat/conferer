
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.ListSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended

data Thing = Thing
  { thingA :: Int
  , thingB :: String
  } deriving (Generic, Show, Eq)

instance FromConfig Thing

spec :: Spec
spec = do
  context "Basic fetching" $ do
    describe "list parsing" $ do
      ensureEmptyConfigThrows @[Int]
      ensureWrongTypeDefaultThrows @[Int]
      ensureUsingDefaultReturnsSameValue @[Int] [7]

      context "with an empty keys it always gets an empty list" $ do
        ensureFetchParses
          @[Int]
          [ ("keys", "")
          ]
          [ ("", toDyn @[Int] [1, 2, 3]) ]
          []
      context "with a fully defined list without a default" $ do
        ensureFetchParses
          @[Int]
          [ ("0", "2")
          ]
          [] 
          [2]

      context "with a non number key" $ do
        ensureFetchParses
          @[Int]
          [ ("a", "0")
          , ("b", "1")
          ]
          []
          [0, 1]

      context "orders the values lexicographically" $ do
        ensureFetchParses
          @[Int]
          [ ("09", "0")
          , ("10", "1")
          ]
          []
          [0, 1]
      context "with a prototype as default" $ do
        ensureFetchParses
          @[Thing]
          [ ("0.a", "0")
          , ("1.b", "aaa")
          ]
          [ ("prototype", toDyn (Thing 7 "lala"))
          ]
          [ Thing { thingA = 0, thingB = "lala"}
          , Thing { thingA = 7, thingB = "aaa"}
          ]

      context "with a partial prototype that gets completed later" $ do
        ensureFetchParses
          @[Thing]
          [ ("0.a", "0")
          , ("prototype.b", "lala")
          ]
          []
          [ Thing { thingA = 0, thingB = "lala"}
          ]

      context "when the user updates the prototype via config" $ do
        ensureFetchParses
          @[Thing]
          [ ("0.a", "0")
          , ("prototype.b", "aaa")
          ]
          [("prototype", toDyn Thing {thingA = 7, thingB = "lala"})
          ]
          [ Thing { thingA = 0, thingB = "aaa"}
          ]
      context "listing keys explicitly uses keys in that order" $ do
        ensureFetchParses
          @[Thing]
          [ ("1.a", "0")
          , ("1.b", "a")
          , ("a.a", "1")
          , ("a.b", "b")
          , ("keys", "1,a")
          ]
          [ ]
          [ Thing 0 "a"
          , Thing 1 "b"
          ]
      context "listing keys explicitly uses the prototype if the key is not complete" $ do
        ensureFetchParses
          @[Thing]
          [ ("1.a", "0")
          , ("keys", "1")
          ]
          [ ("prototype", toDyn $ Thing 7 "lele")
          ]
          [ Thing 0 "lele"
          ]
      context "listing keys explicitly I can use values from the default" $ do
        ensureFetchParses
          @[Thing]
          [ ("keys", "defaults.0")
          ]
          [ ("", toDyn 
              [ Thing {thingA = 0, thingB = "a"}
              , Thing {thingA = 1, thingB = "b"}
              ])
          ]
          [ Thing 0 "a"
          ]
      context "listing keys explicitly I can use values from the default \
              \ and override some of the values" $ do
        ensureFetchParses
          @[Thing]
          [ ("keys", "defaults.0")
          , ("defaults.0.a", "7")
          ]
          [ ("", toDyn 
              [ Thing {thingA = 0, thingB = "a"}
              , Thing {thingA = 1, thingB = "b"}
              ])
          ]
          [ Thing 7 "a"
          ]
      context "listing keys explicitly and using a default from an index \
              \that's not present" $ do
        ensureFetchThrows
          @[Thing]
          [ ("keys", "defaults.5")
          ]
          [ ("", toDyn 
              [ Thing {thingA = 0, thingB = "a"}
              , Thing {thingA = 1, thingB = "b"}
              ])
          ]
          $ aMissingRequiredKey @Int "some.key.defaults.5.a"
      context "listing a default keys that's not present ignores the \
              \prototype" $ do
        ensureFetchThrows
          @[Thing]
          [ ("keys", "defaults.5")
          ]
          [ ("prototype", toDyn $ Thing 7 "lele")
          , ("", toDyn 
              [ Thing {thingA = 0, thingB = "a"}
              , Thing {thingA = 1, thingB = "b"}
              ])
          ]
          $ aMissingRequiredKey @Int "some.key.defaults.5.a"
      context "jjjj" $ do
        ensureFetchParses
          @[[Int]]
          [ ("1.primero", "0")
          -- , ("1.segundomagico", "8")

          , ("prototype.segundomagico", "8")
          ]
          [ 
          ]
          [[0, 8]]