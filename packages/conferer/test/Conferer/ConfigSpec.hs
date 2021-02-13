{-# LANGUAGE TypeApplications #-}
module Conferer.ConfigSpec where

import Test.Hspec
import Data.Text (Text)
import Data.Dynamic

import Conferer.Config
import Conferer.Source.InMemory
import Conferer.FromConfig.Internal (fromDynamics)

missingKey :: Key -> KeyLookupResult -> Bool 
missingKey expectedKey (MissingKey k) = [expectedKey] == k
missingKey _ _ = False

foundInSources :: Key -> Text -> KeyLookupResult -> Bool 
foundInSources expectedKey expected (FoundInSources k t) =  
  expected == t && expectedKey == k
foundInSources _keyExpected _expected _ =  
  False

foundInDefaults :: forall expected. (Eq expected, Typeable expected)
  => Key -> expected -> KeyLookupResult -> Bool 
foundInDefaults expectedKey expected (FoundInDefaults k d) =  
  Just expected == fromDynamics @expected d && expectedKey == k
foundInDefaults _expctedKey _expected _ =  
  False

spec :: Spec
spec = do
  describe "Config" $ do
    let mkConfig keyMappings defaults content = 
          pure (emptyConfig
                  & addKeyMappings keyMappings
                  & addDefaults defaults)
              >>= addSource (fromConfig content)
    describe "#getKey" $ do
      it "getting a non existent key returns missing key" $ do
        c <- mkConfig [] [] []
        res <- getKey "aaa" c
        res `shouldSatisfy` missingKey "aaa"

      it "getting a key only present in the defaults" $ do
        c <- mkConfig [] [("some.key", toDyn @Int 1)] []
        res <- getKey "some.key" c
        res `shouldSatisfy` foundInDefaults "some.key" (1 :: Int)

      context "with multiple sources and defaults" $ do
        let mkConfig' defaults sourceWithPriority otherSource =
              pure (emptyConfig
                & addDefaults defaults)
              >>= addSource (fromConfig sourceWithPriority)
              >>= addSource (fromConfig otherSource)

        it "getting an key returns unwraps the original map" $ do
          c <- mkConfig' [] [] [("some.key", "1")]
          res <- getKey "some.key" c
          res `shouldSatisfy` foundInSources "some.key" "1"

        it "getting an existent key returns in the bottom maps gets it" $ do
          c <- mkConfig' [] [("some.key", "2")] [("some.key", "1")]
          res <- getKey "some.key" c
          res `shouldSatisfy` foundInSources "some.key" "2"
      describe "with some key mapping" $ do
        context "with basic one to one mapping" $ 
          it "gets the value through a mapping" $ do
            c <- mkConfig
                  [ ("something", "server") ]
                  []
                  [ ("server", "aaa") ]
            res <- getKey "something" c
            res `shouldSatisfy` foundInSources "server" "aaa"
        context "with a nested key mapping" $ do
          it "goes through all the mappings and gets the right value" $ do
            c <- mkConfig
                  [ ("something", "else")
                  , ("else", "server")
                  ]
                  []
                  [ ("server", "aaa")
                  ]
            res <- getKey "something" c
            res `shouldSatisfy` foundInSources "server" "aaa"
        context "with circular mappings" $ do
          it "gets the first key" $ do
            c <- mkConfig
                  [ ("a", "b")
                  , ("b", "a")
                  , ("b", "c")
                  ]
                  []
                  [ ("c", "aaa")
                  ]
            res <- getKey "a" c
            res `shouldSatisfy` foundInSources "c" "aaa"
        context "with nested key" $ do
          it "maps the right upper key" $ do
            c <- mkConfig
                  [ ("a", "b")
                  ]
                  []
                  [ ("b.k", "aaa")
                  ]
            res <- getKey "a.k" c
            res `shouldSatisfy` foundInSources "b.k" "aaa"
        context "with some defaults" $ do
          it "maps and allows getting the default" $ do
            c <- mkConfig
                  [ ("a", "b")
                  ]
                  [ ("b", toDyn False)
                  ]
                  []
            res <- getKey "a" c
            res `shouldSatisfy` foundInDefaults "a" False
    describe "#listSubkeys" $ do
      it "return an empty list when nothing is configured" $ do
        c <- mkConfig [] [] []
        res <- listSubkeys "some" c
        res `shouldBe` []
      context "with some values in the config" $ do
        it "when nothing is configured" $ do
          c <- mkConfig [] [] [("some.key", "")]
          res <- listSubkeys "some" c
          res `shouldBe` ["some.key"]
      context "with a config that has a value configured and \
              \we get that same value" $ do
        it "returns that key in the list of subkeys" $ do
          c <- mkConfig [] [] [("some", "")]
          res <- listSubkeys "some" c
          res `shouldBe` []
      context "with a config that has a value configured but \
              \we get a different value" $ do
        it "returns an empty list" $ do
          c <- mkConfig [] [] [("some", "")]
          res <- listSubkeys "other" c
          res `shouldBe` []
      context "with a config with defaults" $ do
        it "returns those defaults in the list" $ do
          c <- mkConfig [] [("some.key", toDyn @Int 7)] []
          res <- listSubkeys "some" c
          res `shouldBe` ["some.key"]
      context "with configs in both defaults and sources" $ do
        it "returns both results combined" $ do
          c <- mkConfig [] [("some.key", toDyn @Int 7)] [("some.other", "")]
          res <- listSubkeys "some" c
          res `shouldBe` ["some.key", "some.other"]
      context "mappings" $ do
        context "with a simple mapping and a configured value" $ do
          it "returns the keys that are present based on the mapping" $ do
            c <- mkConfig
                  [ ("a", "b")
                  ]
                  []
                  [ ("b.k", "aaa")
                  ]
            res <- listSubkeys "a" c
            res `shouldBe` ["a.k"]
        context "with a multiple mappings and a configured value" $ do
          it "returns the right keys" $ do
            c <- mkConfig
                  [ ("a", "b")
                  , ("b", "c")
                  ]
                  []
                  [ ("c.k", "aaa")
                  ]
            res <- listSubkeys "a" c
            res `shouldBe` ["a.k"]
        context "with circular mappings" $ do
          it "returns the right keys" $ do
            c <- mkConfig
                  [ ("a", "b")
                  , ("b", "a")
                  ]
                  []
                  [ ("a.k", "aaa")
                  ]
            res <- listSubkeys "a" c
            res `shouldBe` ["a.k"]
        context "with reducing mappings" $ do
          xit "returns the right keys" $ do
            c <- mkConfig
                  [ ("a.k", "a")
                  ]
                  []
                  [ ("a.k", "aaa")
                  ]
            res <- listSubkeys "a" c
            -- getKey "a.k.k" c >>= print
            res `shouldBe` ["a.k", "a.k.k"]
        it "keys that are present based on the mapping" $ do
            c <- mkConfig
                  [ ("a", "b")
                  ]
                  [ ("b.k", toDyn @String "aaa")
                  ]
                  [ ]
            res <- listSubkeys "a" c
            res `shouldBe` ["a.k"]

    describe "#addDefaults" $ do
      context "with multiple defaults on the same key" $ do
        it "the last one has more priority" $ do
          let c = emptyConfig
                & addDefaults
                  [ ("some.key", toDyn @Int 1)
                  , ("some.key", toDyn @Int 2)
                  ]
          res <- getKey "some.key" c
          res `shouldSatisfy` foundInDefaults "some.key" (2 :: Int)
