{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Example showing how to use the documentation features in Conferer
--
-- This example demonstrates:
-- 1. How to create documentation for your config keys
-- 2. How to use explainKey to show documentation for a specific key (like kubectl explain)
-- 3. How to generate a full schema/help document
module DocumentationExample where

import qualified Conferer
import qualified Data.Text.IO as T
import GHC.Generics

-- | Sample application configuration
data AppConfig = AppConfig
  { appConfigServerPort :: Int
  , appConfigServerHost :: String
  , appConfigDatabaseUrl :: String
  , appConfigLogLevel :: String
  , appConfigMaxConnections :: Int
  } deriving (Generic, Show)

instance Conferer.FromConfig AppConfig
instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigServerPort = 8080
    , appConfigServerHost = "localhost"
    , appConfigDatabaseUrl = "postgres://localhost/mydb"
    , appConfigLogLevel = "info"
    , appConfigMaxConnections = 100
    }

-- | Create documentation for the application configuration
--
-- This shows different ways to add documentation:
-- - Simple descriptions
-- - With type information
-- - With examples
-- - With default values
-- - Full documentation
createAppDocs :: Conferer.ConfigDocs
createAppDocs =
  Conferer.emptyDocs
    -- Simple documentation
    & Conferer.doc "server.port" "Port number for the HTTP server to listen on"
    & Conferer.doc "server.host" "Host address to bind the HTTP server to"

    -- Documentation with type information
    & Conferer.docWithType
        "database.url"
        "PostgreSQL connection string for the application database"
        "String"

    -- Documentation with example
    & Conferer.docWithExample
        "log.level"
        "Logging level for the application"
        "debug, info, warn, or error"

    -- Documentation with default value
    & Conferer.docWithDefault
        "max.connections"
        "Maximum number of concurrent database connections"
        "100"

    -- Full documentation (all fields)
    & Conferer.docFull
        "api.key"
        "API key for external service authentication"
        "String"
        "(none - must be provided)"
        "sk_live_1234567890abcdef"

-- | Alternative way to create documentation using helper syntax
createAppDocsAlternative :: Conferer.ConfigDocs
createAppDocsAlternative = mconcat
  [ Conferer.doc "server.port" "Port number for the HTTP server"
  , Conferer.doc "server.host" "Host address for the HTTP server"
  , Conferer.docWithType "database.url" "Database connection string" "String"
  , Conferer.docWithExample "log.level" "Logging level" "info"
  , Conferer.docWithDefault "max.connections" "Max DB connections" "100"
  ]

main :: IO ()
main = do
  putStrLn "=== Conferer Documentation Example ===\n"

  -- Create configuration
  config <- Conferer.mkConfig "docexample"

  -- Create documentation
  let docs = createAppDocs

  putStrLn "1. Explaining a specific key (like kubectl explain):"
  putStrLn "---------------------------------------------------"
  explanation <- Conferer.explainKey config docs "server.port"
  T.putStrLn explanation

  putStrLn "\n2. Explaining another key with full documentation:"
  putStrLn "---------------------------------------------------"
  explanation2 <- Conferer.explainKey config docs "api.key"
  T.putStrLn explanation2

  putStrLn "\n3. Generate complete schema documentation:"
  putStrLn "---------------------------------------------------"
  let schema = Conferer.generateSchema docs
  T.putStrLn schema

  putStrLn "\n4. Fetching actual config values:"
  putStrLn "---------------------------------------------------"
  appConfig <- Conferer.fetch config :: IO AppConfig
  putStrLn $ "Server will run on: " ++ appConfigServerHost appConfig
    ++ ":" ++ show (appConfigServerPort appConfig)
  putStrLn $ "Log level: " ++ appConfigLogLevel appConfig

  putStrLn "\n=== Usage Examples ===\n"
  putStrLn "You can set these values via:"
  putStrLn "  - Environment variables: DOCEXAMPLE_SERVER_PORT=3000"
  putStrLn "  - CLI args: --server.port=3000"
  putStrLn "  - Config files: config/development.properties"
  putStrLn ""
  putStrLn "Try running:"
  putStrLn "  DOCEXAMPLE_SERVER_PORT=9000 ./example"
  putStrLn "  ./example --server.host=0.0.0.0"

-- | Helper function demonstrating programmatic use
-- This could be used to create a CLI tool like:
--   myapp config explain server.port
--   myapp config schema
explainKeyCommand :: String -> IO ()
explainKeyCommand keyName = do
  config <- Conferer.mkConfig "docexample"
  let docs = createAppDocs
  explanation <- Conferer.explainKey config docs (Conferer.mkKey keyName)
  T.putStrLn explanation

-- | Generate schema command
-- Usage: myapp config schema > schema.md
generateSchemaCommand :: IO ()
generateSchemaCommand = do
  let docs = createAppDocs
      schema = Conferer.generateSchema docs
  T.putStrLn schema
