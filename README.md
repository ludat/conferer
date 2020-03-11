<h1 align="center">Conferer</h1>
<p align="center">
    <a href="https://img.shields.io/travis/ludat/conferer" alt="Travis CI">
        <img src="https://img.shields.io/travis/ludat/conferer" />
    </a>
    <a href="https://img.shields.io/hackage/v/conferer" alt="Hackage version">
        <img src="https://img.shields.io/hackage/v/conferer" />
    </a>
    <a href="https://img.shields.io/hackage-deps/v/conferer" alt="Hackage deps">
        <img src="https://img.shields.io/hackage-deps/v/conferer" />
    </a>
</p>

## The problem

Have you ever tried configuring a Haskell application? If you are not the author
you are usually out of luck and the only way to configure it is recompiling, and
even if you are the author you have to write that logic yourself (reading env vars,
files or cli params), what about partial updates? and environments? or error handling?

## One solution: Conferer

Conferer is a library that defines ways of getting configuration for your
Haskell application and the libraries it uses in a very ergonomical way.

## Example: one Settings

Let's say I want to configure a warp server, then we'd do:

```haskell
main = do
  -- First we create a Config, which defines which sources our config will be
  -- reading, by default cli params, env vars and .properties files
  config <- defaultConfig "awesomeapp"
  -- Then we use getFromConfig with some arbitrary key (to scope the server
  -- config) and we use our Config to generate a Warp Settings
  warpConfig :: Warp.Settings <- getFromConfig "server" config

  -- Afterwards we use the Settings as usual
  Warp.runSettings warpConfig myApp
```

Now I need to chage the port of the app, I can change it by either:

* Setting cli params like `./myApp --server.port=5555`
* Setting an environment variable called `AWESOMEAPP_SERVER_PORT=5555`
* In a `config/dev.properties` file, you can have `server.port=5555`

And you may also get that value from different configuration providers like
redis, json file, dhall file or whichever you may need.

## Example 2: many different values with defaults

Let's say I want to configure a warp server and a redis db (using hedis), To 
do that we'd do:

```haskell

-- First we create our configuration record which holds all the configurations
-- our app needs
data AppConfig = AppConfig
  { appConfigWarp :: Settings
  -- ^- From Warp
  , appConfigHedis :: ConnectionInfo
  -- ^- From Hedis
  , appConfigSecret :: Text
  -- ^- Some custom value we need
  } deriving (Generic)
  -- ^- We need to derive Generic to derive FromConfig

-- This typeclass defines how to create our type from a bunch of string based
-- key/values, (which our Config is), for records we can derive it using
-- Generics
instance FromConfig AppConfig

-- Now we need a default value for our app, all apps should be able to work
-- at least somewhat stupidly even if the user doens't supply configurations
-- at all
instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigWarp = setPort 2222 configDef
    -- ^- We want the default Warp config but the port should be 2222
    --    if the config doesn't mention it
    , appConfigHedis = configDef 
    -- ^- defaults for hedis are ok
    , appConfigSecret = "very secret... shhh"
    -- ^- we decide some random default, notice that Text has no default
    --    so using configDef here won't compile
    }


main = do
  -- Like last time we create the config
  config <- defaultConfig "awesomeapp"
  -- Then we use getFromRootConfig without a key since Generics on AppConfig
  -- already scoped everything inside itself and we use our Config to
  -- generate an AppConfig
  warpConfig :: AppConfig <- getFromRootConfig config

  -- Afterwards we use the Settings as usual
  Warp.runSettings warpConfig myApp
```

Now to configure our app we can use the same sources as before (env vars, cli,
files, etc) but using the following flags we can configure:

* `--warp.port=5555`: set warp's server port to 5555
* `--secret=real_secrets`: set our custom secret to `"real_secrets"`
* `--hedis=redis://username:password@host:42/2`: set hedis' connection to that
* `--hedis.host=redis.example.com`: set hedis' connection host to `redis.example.com`


## Existing providers

Providers usually incur in many dependencies so they are split into different
packages

* *[Json](https://hackage.haskell.org/package/conferer-provider-json)* (depends on `aeson`)
* *[Dhall](https://hackage.haskell.org/package/conferer-provider-dhall)* (depends on `dhall`)
* *[Yaml](https://hackage.haskell.org/package/conferer-provider-yaml)* (depends on `yaml`) 

## Existing FromConfig instances

Default instances for fetching a values from a config (usually a config value
for some library)

* *[snap-server](https://hackage.haskell.org/package/conferer-snap)*
* *[warp](https://hackage.haskell.org/package/conferer-warp)*
* *[hedis](https://hackage.haskell.org/package/conferer-hedis)*
* *[hspec](https://hackage.haskell.org/package/conferer-hspec)*

## Utilities

There are as well some utilities to change providers:

* `Conferer.Provider.Namespace`: All keys must be namespaced and the namespace
  is striped for lookup
* `Conferer.Provider.Mapped`: Using a map key to maybe key you can change the
  name of a key or even hiding some key
* `Conferer.Provider.Simple`: Get keys from a hardcoded map key to string

## Future maybe things

* Interpolate keys with other keys: `{a: "db", b: "${a}_thing"}`, getting `b`
  will give `"db_thing"` (maybe) even in different levels of configuration
* A LOT of providers
* A LOT of `FromConfig` implementations
