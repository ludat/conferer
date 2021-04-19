{-# LANGUAGE TypeApplications #-}
module Conferer.ConfigSpec where

import Test.Hspec
import Data.Dynamic

import Conferer.Config
import Conferer.Source.InMemory

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
        res <- getKeyFromSources "aaa" c
        res `shouldBe` MissingKey () ["aaa"]

      it "getting a key only present in the defaults" $ do
        c <- mkConfig [] [("some.key", toDyn @Int 1)] []
        getKeyFromDefaults @Int "some.key" c
          `shouldBe` FoundInDefaults 1 "some.key"

      context "with multiple sources and defaults" $ do
        let mkConfig' defaults sourceWithPriority otherSource =
              pure (emptyConfig
                & addDefaults defaults)
              >>= addSource (fromConfig sourceWithPriority)
              >>= addSource (fromConfig otherSource)

        it "getting an key returns unwraps the original map" $ do
          c <- mkConfig' [] [] [("some.key", "1")]
          res <- getKeyFromSources "some.key" c
          res `shouldBe` FoundInSources "1" "some.key"

        it "getting an existent key returns in the bottom maps gets it" $ do
          c <- mkConfig' [] [("some.key", "2")] [("some.key", "1")]
          res <- getKeyFromSources "some.key" c
          res `shouldBe` FoundInSources "2" "some.key"
      describe "with some key mapping" $ do
        context "with basic one to one mapping" $
          it "gets the value through a mapping" $ do
            c <- mkConfig
                  [ ("something", "server") ]
                  []
                  [ ("server", "aaa") ]
            res <- getKeyFromSources "something" c
            res `shouldBe` FoundInSources "aaa" "server"
        context "with a nested key mapping" $ do
          it "goes through all the mappings and gets the right value" $ do
            c <- mkConfig
                  [ ("something", "else")
                  , ("else", "server")
                  ]
                  []
                  [ ("server", "aaa")
                  ]
            res <- getKeyFromSources "something" c
            res `shouldBe` FoundInSources "aaa" "server"
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
            res <- getKeyFromSources "a" c
            res `shouldBe` FoundInSources "aaa" "c"
        context "with nested key" $ do
          it "maps the right upper key" $ do
            c <- mkConfig
                  [ ("a", "b")
                  ]
                  []
                  [ ("b.k", "aaa")
                  ]
            res <- getKeyFromSources "a.k" c
            res `shouldBe` FoundInSources "aaa" "b.k"
        context "with some defaults" $ do
          it "maps and allows getting the default" $ do
            c <- mkConfig
                  [ ("a", "b")
                  ]
                  [ ("b", toDyn False)
                  ]
                  []
            getKeyFromDefaults @Bool "a" c
              `shouldBe` FoundInDefaults False "b"
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
        xit "returns those defaults in the list" $ do
          c <- mkConfig [] [] [("some.key", "7")]
          res <- listSubkeys "some" c
          res `shouldBe` ["some.key"]
      context "with configs in both defaults and sources" $ do
        xit "returns both results combined" $ do
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
                  [ ]
                  [ ("b.k", "aaa")
                  ]
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
          getKeyFromDefaults @Int "some.key" c
            `shouldBe` FoundInDefaults 2 "some.key"

    describe "#addDefault" $ do
      context "with multiple defaults on the same key" $ do
        it "the last one has more priority" $ do
          let c = emptyConfig
                & addDefault @Int "some.key" 1
                & addDefault @Int "some.key" 2
          getKeyFromDefaults @Int "some.key" c
            `shouldBe` FoundInDefaults 2 "some.key"
