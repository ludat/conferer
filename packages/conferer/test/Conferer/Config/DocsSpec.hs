{-# LANGUAGE OverloadedStrings #-}
module Conferer.Config.DocsSpec where

import Test.Hspec
import qualified Data.Text as Text
import qualified Data.Map as Map

import Conferer
import Conferer.Config.Docs
import Conferer.Source.InMemory

spec :: Spec
spec = do
  describe "ConfigDocs" $ do
    describe "emptyDocs" $ do
      it "creates empty documentation" $ do
        let docs = emptyDocs
        getConfigDocs docs `shouldBe` Map.empty

    describe "doc" $ do
      it "adds simple documentation for a key" $ do
        let docs = doc "server.port" "Server port number" emptyDocs
            keyDoc = Map.lookup "server.port" (getConfigDocs docs)
        fmap keyDocDescription keyDoc `shouldBe` Just (Just "Server port number")

    describe "docWithType" $ do
      it "adds documentation with type information" $ do
        let docs = docWithType "db.url" "Database URL" "String" emptyDocs
            keyDoc = Map.lookup "db.url" (getConfigDocs docs)
        fmap keyDocDescription keyDoc `shouldBe` Just (Just "Database URL")
        fmap keyDocType keyDoc `shouldBe` Just (Just "String")

    describe "docWithExample" $ do
      it "adds documentation with example" $ do
        let docs = docWithExample "log.level" "Logging level" "debug" emptyDocs
            keyDoc = Map.lookup "log.level" (getConfigDocs docs)
        fmap keyDocDescription keyDoc `shouldBe` Just (Just "Logging level")
        fmap keyDocExample keyDoc `shouldBe` Just (Just "debug")

    describe "docWithDefault" $ do
      it "adds documentation with default value" $ do
        let docs = docWithDefault "timeout" "Request timeout" "30s" emptyDocs
            keyDoc = Map.lookup "timeout" (getConfigDocs docs)
        fmap keyDocDescription keyDoc `shouldBe` Just (Just "Request timeout")
        fmap keyDocDefault keyDoc `shouldBe` Just (Just "30s")

    describe "docFull" $ do
      it "adds complete documentation" $ do
        let docs = docFull "api.key" "API key" "String" "none" "sk_live_123" emptyDocs
            keyDoc = Map.lookup "api.key" (getConfigDocs docs)
        keyDoc `shouldBe` Just (KeyDoc
          { keyDocDescription = Just "API key"
          , keyDocType = Just "String"
          , keyDocExample = Just "sk_live_123"
          , keyDocDefault = Just "none"
          })

    describe "Monoid instance" $ do
      it "combines multiple docs" $ do
        let docs1 = doc "key1" "Description 1" emptyDocs
            docs2 = doc "key2" "Description 2" emptyDocs
            combined = docs1 <> docs2
        Map.size (getConfigDocs combined) `shouldBe` 2

  describe "explainKey" $ do
    it "explains a key with documentation" $ do
      config <- mkConfig' mempty [fromMap [("server.port", "8080")]]
      let docs = doc "server.port" "HTTP server port" emptyDocs
      explanation <- explainKey config docs "server.port"
      explanation `shouldSatisfy` Text.isInfixOf "server.port"
      explanation `shouldSatisfy` Text.isInfixOf "HTTP server port"
      explanation `shouldSatisfy` Text.isInfixOf "8080"

    it "explains a key without documentation" $ do
      config <- mkConfig' mempty []
      let docs = emptyDocs
      explanation <- explainKey config docs "unknown.key"
      explanation `shouldSatisfy` Text.isInfixOf "unknown.key"
      explanation `shouldSatisfy` Text.isInfixOf "No documentation available"

    it "shows type information when available" $ do
      config <- mkConfig' mempty []
      let docs = docWithType "port" "Server port" "Int" emptyDocs
      explanation <- explainKey config docs "port"
      explanation `shouldSatisfy` Text.isInfixOf "Int"

  describe "generateSchema" $ do
    it "generates empty schema for no docs" $ do
      let docs = emptyDocs
          schema = generateSchema docs
      schema `shouldSatisfy` Text.isInfixOf "No configuration"

    it "generates schema with all documented keys" $ do
      let docs = emptyDocs
            & doc "server.port" "HTTP server port"
            & doc "server.host" "HTTP server host"
            & docWithType "db.url" "Database connection string" "String"
          schema = generateSchema docs
      schema `shouldSatisfy` Text.isInfixOf "server.port"
      schema `shouldSatisfy` Text.isInfixOf "server.host"
      schema `shouldSatisfy` Text.isInfixOf "db.url"
      schema `shouldSatisfy` Text.isInfixOf "HTTP server port"
      schema `shouldSatisfy` Text.isInfixOf "Database connection string"

    it "includes type information in schema" $ do
      let docs = docWithType "timeout" "Request timeout" "Int" emptyDocs
          schema = generateSchema docs
      schema `shouldSatisfy` Text.isInfixOf "Type"
      schema `shouldSatisfy` Text.isInfixOf "Int"

    it "includes default values in schema" $ do
      let docs = docWithDefault "retries" "Number of retries" "3" emptyDocs
          schema = generateSchema docs
      schema `shouldSatisfy` Text.isInfixOf "Default"
      schema `shouldSatisfy` Text.isInfixOf "3"

    it "includes examples in schema" $ do
      let docs = docWithExample "format" "Output format" "json" emptyDocs
          schema = generateSchema docs
      schema `shouldSatisfy` Text.isInfixOf "Example"
      schema `shouldSatisfy` Text.isInfixOf "json"
