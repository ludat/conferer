{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Conferer.ExceptionMessagesSpec where

import Test.Hspec
import Test.Hspec.Golden
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable

import Conferer
import Conferer.Config
import Conferer.FromConfig
import Conferer.Source
import Conferer.Source.Env (fromEnvList)
import Conferer.Source.CLIArgs (fromArgs)
import Conferer.Source.PropertiesFile (fromFileContent)
import Conferer.Source.Null (NullSource(..))
import Tests

spec :: Spec
spec = do
  describe "ConfigParsingError exception messages" $ do
    $(betterGolden) "formats Int parsing error from env source" $ do
      config <- mkConfigWithEnv [("APP_PORT", "not_a_number")]
      captureException $ fetchFromConfig @Int "port" config

    $(betterGolden) "formats Bool parsing error from CLI args" $ do
      config <- mkConfigWithCLI ["--enabled=maybe"]
      captureException $ fetchFromConfig @Bool "enabled" config

    $(betterGolden) "formats Int parsing error from properties file" $ do
      config <- mkConfigWithProperties "/tmp/test.properties" "timeout=invalid"
      captureException $ fetchFromConfig @Int "timeout" config

  describe "MissingRequiredKey exception messages" $ do
    $(betterGolden) "formats single key missing from env source" $ do
      config <- mkConfigWithEnv []
      captureException $ fetchFromConfig @String "database.host" config

    $(betterGolden) "formats single key missing from CLI source" $ do
      config <- mkConfigWithCLI []
      captureException $ fetchFromConfig @Int "server.port" config

    $(betterGolden) "formats single key missing from properties file" $ do
      config <- mkConfigWithProperties "/etc/app.properties" ""
      captureException $ fetchFromConfig @String "api.key" config

    $(betterGolden) "formats single key missing from non-existent properties file" $ do
      config <- mkConfigWithNonExistentProperties "/path/to/missing.properties"
      captureException $ fetchFromConfig @String "redis.host" config

    $(betterGolden) "formats single key missing from multiple sources" $ do
      config <- mkConfigWithMultipleSources
      captureException $ fetchFromConfig @Int "cache.ttl" config

    $(betterGolden) "formats missing key with complex type" $ do
      config <- mkConfigWithEnv []
      captureException $ fetchFromConfig @[String] "allowed.hosts" config

mkConfigWithEnv :: [(String, String)] -> IO Config
mkConfigWithEnv envVars =
  addSource (\_ -> return $ fromEnvList envVars "APP") emptyConfig

mkConfigWithCLI :: [String] -> IO Config
mkConfigWithCLI args =
  addSource (\_ -> return $ fromArgs args) emptyConfig

mkConfigWithProperties :: FilePath -> Text -> IO Config
mkConfigWithProperties path content =
  addSource (\_ -> return $ fromFileContent path content) emptyConfig

mkConfigWithNonExistentProperties :: FilePath -> IO Config
mkConfigWithNonExistentProperties path = do
  -- Use Null source with a custom explanation that simulates non-existent file
  let mkSource = \_ -> return $ Source $ NullSource $ \key ->
        concat
        [ "Creating a file '"
        , path
        , "' (it doesn't exist now) and adding a line '"
        , Text.unpack $ Text.intercalate "." $ rawKeyComponents key
        , "=some value'."
        ]
  addSource mkSource emptyConfig

mkConfigWithMultipleSources :: IO Config
mkConfigWithMultipleSources = do
  config1 <- addSource (\_ -> return $ fromEnvList [] "APP") emptyConfig
  config2 <- addSource (\_ -> return $ fromArgs []) config1
  addSource (\_ -> return $ fromFileContent "/etc/app.properties" "") config2
