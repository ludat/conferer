{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
module Conferer.Source.ExplanationsSpec where

import Test.Hspec
import Test.Hspec.Golden
import qualified Data.Text as Text

import Conferer.Source
import Tests
import Conferer.Source.Env (fromEnvList)
import Conferer.Source.CLIArgs (fromArgs)
import Conferer.Source.PropertiesFile (fromFileContent)
import Conferer.Source.Namespaced (fromInner)
import Conferer.Source.Null (NullSource(..))

spec :: Spec
spec = do
  describe "Source explainNotFound" $ do
    $(betterGolden) "explains how to set a key in EnvSource" $ do
      let source = fromEnvList [] "APP"
          explanation = explainNotFound source "database.host"
      pure explanation

    $(betterGolden)  "explains how to set a nested key in EnvSource" $ do
      let source = fromEnvList [] "MYAPP"
          explanation = explainNotFound source "server.ssl.enabled"
      pure explanation

    $(betterGolden) "explains how to set a key in CLIArgsSource" $ do
      let source = fromArgs []
          explanation = explainNotFound source "server.port"
      pure explanation

    $(betterGolden) "explains how to set a nested key in CLIArgsSource" $ do
      let source = fromArgs []
          explanation = explainNotFound source "database.connection.timeout"
      pure explanation

    $(betterGolden) "explains how to set a key in PropertiesFileSource" $ do
      let source = fromFileContent "/etc/app.properties" ""
      pure $ explainNotFound source "api.key"

    $(betterGolden) "explains how to set a key in non-existent PropertiesFileSource" $ do
      let source = Source $ NullSource $ const "IMPOSSIBLE"
      pure $ explainNotFound source "redis.host"

    $(betterGolden) "explains how to set a key in NamespacedSource wrapping Env" $ do
      let envSource = fromEnvList [] "APP"
          source = fromInner "worker" envSource
          explanation = explainNotFound source "queue.name"
      pure explanation

    $(betterGolden) "explains a found key in EnvSource" $ do
      let source = fromEnvList [("APP_DATABASE_HOST", "localhost")] "APP"
          explanation = explainSettedKey source "database.host"
      pure explanation

    $(betterGolden) "explains a found key in CLIArgsSource" $ do
      let source = fromArgs ["--server.port=8080"]
          explanation = explainSettedKey source "server.port"
      pure explanation

    $(betterGolden) "explains a found key in PropertiesFileSource" $ do
      let source = fromFileContent "/etc/app.properties" "api.key=secret123"
          explanation = explainSettedKey source "api.key"
      pure explanation

    $(betterGolden) "explains a found nested key in EnvSource" $ do
      let source = fromEnvList [("APP_SERVER_SSL_ENABLED", "true")] "APP"
          explanation = explainSettedKey source "server.ssl.enabled"
      pure explanation

    $(betterGolden) "explains a found key in NamespacedSource" $ do
      let envSource = fromEnvList [("APP_WORKER_QUEUE_NAME", "jobs")] "APP"
          source = fromInner "worker" envSource
          explanation = explainSettedKey source "queue.name"
      pure explanation
