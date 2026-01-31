{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Conferer.MultiSourceErrorsSpec where

import Test.Hspec
import Test.Hspec.Golden
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as Text

import Conferer
import Conferer.Config
import Conferer.Source.Env (fromEnvList)
import Conferer.Source.CLIArgs (fromArgs)
import Conferer.Source.PropertiesFile (fromFileContent)
import Conferer.Source.InMemory (fromAssociations, ExplainNotFound (..), ExplainSettedKey (..))
import Tests

spec :: Spec
spec = do
  describe "Multi-source error messages" $ do
    $(betterGolden) "shows all sources when key is missing from env + CLI + properties" $ do
      putStrLn "lelle"
      config <- mkConfigTripleSource
      captureException $ fetchFromConfig @String "database.password" config

    $(betterGolden) "shows correct source when parsing fails in second source" $ do
      config <- mkConfigWithInvalidValueInCLI
      captureException $ fetchFromConfig @Int "server.port" config

    $(betterGolden) "shows all sources for missing nested key" $ do
      config <- mkConfigTripleSource
      captureException $ fetchFromConfig @Int "app.worker.threads" config

    $(betterGolden) "shows combined env, CLI, and in-memory sources" $ do
      config <- mkConfigQuadSource
      captureException $ fetchFromConfig @[String] "allowed.origins" config

    $(betterGolden) "shows parsing error with correct source index from properties file" $ do
      config <- mkConfigWithInvalidValueInProperties
      captureException $ fetchFromConfig @Bool "feature.enabled" config

-- Helper functions

mkConfigTripleSource :: IO Config
mkConfigTripleSource = do
  pure emptyConfig
    >>= addSource (\_ -> return $ fromEnvList [] "APP")
    >>= addSource (\_ -> return $ fromArgs [])
    >>= addSource (\_ -> return $ fromFileContent "/etc/myapp.properties" "")

mkConfigQuadSource :: IO Config
mkConfigQuadSource = do
  -- InMemory source needs explanation functions
  let explainNotF = ExplainNotFound $ \k -> "Setting key '" ++ show k ++ "' in the config"
      explainSetF = ExplainSettedKey $ \k -> "key '" ++ show k ++ "' from config"
  pure emptyConfig
    >>= addSource (\_ -> return $ fromEnvList [] "APP")
    >>= addSource (\_ -> return $ fromArgs [])
    >>= addSource (\_ -> return $ fromFileContent "/etc/myapp.properties" "")
    >>= addSource (\_ -> return $ fromAssociations explainNotF explainSetF [])

mkConfigWithInvalidValueInCLI :: IO Config
mkConfigWithInvalidValueInCLI = do
  config1 <- addSource (\_ -> return $ fromEnvList [] "APP") emptyConfig
  addSource (\_ -> return $ fromArgs ["--server.port=not-a-number"]) config1

mkConfigWithInvalidValueInProperties :: IO Config
mkConfigWithInvalidValueInProperties = do
  config1 <- addSource (\_ -> return $ fromEnvList [] "APP") emptyConfig
  config2 <- addSource (\_ -> return $ fromArgs []) config1
  addSource (\_ -> return $ fromFileContent "/config/app.properties" "feature.enabled=maybe") config2
