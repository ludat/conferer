# Configuration Documentation

**New Feature:** Conferer now supports documenting your configuration keys, similar to `kubectl explain`!

## Overview

The configuration documentation feature allows you to:

1. **Document your configuration keys** - Add descriptions, types, examples, and default values
2. **Explain specific keys** - Show detailed information about a configuration key (like `kubectl explain`)
3. **Generate schema documentation** - Export all configuration documentation to markdown format

This is particularly useful for:
- Large applications with many configuration options
- Team documentation and onboarding
- Generating configuration reference guides
- Self-documenting configuration schemas

## Quick Start

```haskell
import qualified Conferer

-- Create documentation for your config keys
docs :: Conferer.ConfigDocs
docs = Conferer.emptyDocs
  & Conferer.doc "server.port" "Port number for the HTTP server"
  & Conferer.doc "server.host" "Host address to bind to"
  & Conferer.docWithType "database.url" "PostgreSQL connection string" "String"
  & Conferer.docWithExample "log.level" "Logging level" "info, debug, warn, error"
  & Conferer.docWithDefault "max.connections" "Max concurrent connections" "100"

main :: IO ()
main = do
  config <- Conferer.mkConfig "myapp"

  -- Explain a specific key (like kubectl explain)
  explanation <- Conferer.explainKey config docs "server.port"
  putStrLn explanation

  -- Generate full schema documentation
  let schema = Conferer.generateSchema docs
  writeFile "CONFIG_SCHEMA.md" schema
```

## Documentation Functions

### Creating Documentation

#### `emptyDocs :: ConfigDocs`
Creates an empty documentation registry.

```haskell
docs = emptyDocs
```

#### `doc :: Key -> Text -> ConfigDocs -> ConfigDocs`
Add a simple description for a configuration key.

```haskell
docs = doc "api.timeout" "Request timeout in seconds" emptyDocs
```

#### `docWithType :: Key -> Text -> Text -> ConfigDocs -> ConfigDocs`
Add documentation with type information.

```haskell
docs = docWithType "server.port" "HTTP server port" "Int" emptyDocs
```

#### `docWithExample :: Key -> Text -> Text -> ConfigDocs -> ConfigDocs`
Add documentation with an example value.

```haskell
docs = docWithExample "log.format" "Log output format" "json" emptyDocs
```

#### `docWithDefault :: Key -> Text -> Text -> ConfigDocs -> ConfigDocs`
Add documentation with the default value.

```haskell
docs = docWithDefault "retry.count" "Number of retry attempts" "3" emptyDocs
```

#### `docFull :: Key -> Text -> Text -> Text -> Text -> ConfigDocs -> ConfigDocs`
Add complete documentation with all fields.

```haskell
docs = docFull
  "api.key"
  "API key for authentication"
  "String"
  "(required - no default)"
  "sk_live_1234567890abcdef"
  emptyDocs
```

### Querying Documentation

#### `explainKey :: Config -> ConfigDocs -> Key -> IO Text`
Explain a specific configuration key, showing:
- Key name
- Description
- Type information
- Current value and source
- Default value
- Example

```haskell
config <- mkConfig "myapp"
let docs = doc "server.port" "HTTP server port" emptyDocs
explanation <- explainKey config docs "server.port"
putStrLn explanation
```

Output example:
```
Key: server.port
================

Description:
  HTTP server port

Type:
  Int

Current Value:
  8080
  Source: sources[0]

Example:
  3000
```

#### `generateSchema :: ConfigDocs -> Text`
Generate a markdown-formatted schema document containing all documented configuration keys.

```haskell
let docs = emptyDocs
      & doc "server.port" "HTTP server port"
      & doc "server.host" "HTTP server host"
      & docWithType "database.url" "Database connection" "String"
    schema = generateSchema docs
writeFile "CONFIG.md" schema
```

Output example:
```markdown
Configuration Schema
====================

### server.port

HTTP server port

- **Type:** `Int`
- **Default:** `8080`

### server.host

HTTP server host

- **Type:** `String`
- **Default:** `localhost`

### database.url

Database connection

- **Type:** `String`
```

## Usage Patterns

### Pattern 1: Building Documentation Incrementally

```haskell
docs :: ConfigDocs
docs = emptyDocs
  & doc "server.port" "HTTP server port"
  & doc "server.host" "HTTP server host"
  & doc "database.url" "Database connection string"
  & doc "log.level" "Logging verbosity"
```

### Pattern 2: Using Monoid Instance

```haskell
serverDocs :: ConfigDocs
serverDocs = mconcat
  [ doc "server.port" "HTTP port"
  , doc "server.host" "HTTP host"
  ]

dbDocs :: ConfigDocs
dbDocs = mconcat
  [ doc "database.url" "DB connection"
  , doc "database.pool.size" "Connection pool size"
  ]

allDocs :: ConfigDocs
allDocs = serverDocs <> dbDocs
```

### Pattern 3: CLI Commands

Create a CLI tool to explore configuration:

```haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["config", "explain", keyName] -> explainKeyCommand keyName
    ["config", "schema"] -> generateSchemaCommand
    _ -> runApp

explainKeyCommand :: String -> IO ()
explainKeyCommand keyName = do
  config <- mkConfig "myapp"
  let docs = createAppDocs
  explanation <- explainKey config docs (mkKey keyName)
  T.putStrLn explanation

generateSchemaCommand :: IO ()
generateSchemaCommand = do
  let docs = createAppDocs
      schema = generateSchema docs
  T.putStrLn schema
```

Usage:
```bash
$ myapp config explain server.port
$ myapp config schema > CONFIG.md
```

### Pattern 4: Automatic Schema Generation

Generate configuration documentation as part of your build process:

```haskell
-- tools/generate-config-docs.hs
import qualified Conferer
import qualified Data.Text.IO as T

main :: IO ()
main = do
  let docs = createAppDocs  -- Your app's documentation
      schema = Conferer.generateSchema docs
  T.writeFile "docs/configuration.md" schema
```

## Integration Examples

### Example 1: Web Application

```haskell
data AppConfig = AppConfig
  { serverPort :: Int
  , serverHost :: String
  , dbUrl :: String
  , logLevel :: String
  } deriving (Generic)

instance FromConfig AppConfig
instance DefaultConfig AppConfig where
  configDef = AppConfig 8080 "localhost" "postgres://localhost/app" "info"

appDocs :: ConfigDocs
appDocs = emptyDocs
  & docWithDefault "server.port" "HTTP server port" "8080"
  & docWithDefault "server.host" "Server bind address" "localhost"
  & docFull
      "db.url"
      "PostgreSQL connection string"
      "String"
      "postgres://localhost/app"
      "postgres://user:pass@host:5432/dbname"
  & docWithExample
      "log.level"
      "Application logging level"
      "info, debug, warn, error"

main :: IO ()
main = do
  config <- mkConfig "webapp"

  -- Show help if requested
  args <- getArgs
  when ("--help-config" `elem` args) $ do
    T.putStrLn $ generateSchema appDocs
    exitSuccess

  -- Normal application startup
  appConfig <- fetch config :: IO AppConfig
  ...
```

### Example 2: Microservice

```haskell
serviceDocs :: ConfigDocs
serviceDocs = emptyDocs
  & docFull
      "service.name"
      "Service identifier for logging and metrics"
      "String"
      "unknown-service"
      "user-auth-service"
  & docWithType
      "service.port"
      "gRPC server port"
      "Int"
  & docWithDefault
      "observability.metrics.enabled"
      "Enable Prometheus metrics endpoint"
      "true"
  & docWithDefault
      "observability.tracing.enabled"
      "Enable distributed tracing"
      "false"
  & docWithExample
      "observability.tracing.endpoint"
      "OpenTelemetry collector endpoint"
      "http://localhost:4318"
```

## Best Practices

1. **Document all user-facing configuration** - At minimum, add a description for every config key
2. **Include types** - Help users understand what values are expected
3. **Provide examples** - Show realistic example values
4. **Note defaults** - Make it clear what happens if a value isn't provided
5. **Generate schema in CI** - Automatically generate and commit configuration documentation
6. **Version your schema** - Keep configuration docs in sync with code versions
7. **Use namespaces** - Group related configuration keys (e.g., `server.*`, `database.*`)

## Comparison with kubectl explain

The `explainKey` function is inspired by Kubernetes' `kubectl explain` command:

```bash
# Kubernetes
$ kubectl explain pod.spec.containers

# Conferer equivalent
$ myapp config explain pod.spec.containers
```

Both provide:
- Hierarchical key paths
- Type information
- Descriptions
- Current/default values

## Future Enhancements

Potential future additions:
- JSON Schema export
- Validation rules in documentation
- Auto-detection of types from FromConfig instances
- Interactive documentation browser
- Environment variable name resolution

## See Also

- [Getting Started Guide](getting-started.md)
- [Core Concepts](core-concepts.md)
- [FromConfig Guide](fromConfig.md)
- [Example Application](../example/src/DocumentationExample.hs)
