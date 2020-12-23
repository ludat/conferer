---
id: multiple-tutorial
title: Multiple configs and generics
---

## Multiple different configuration

### Our Current app

This example assumes you've read the first tutorial so if you can find it [here](/docs/tutorial).

You have an application which uses [warp](https://hackage.haskell.org/package/warp),
[hedis](https://hackage.haskell.org/package/hedis) and need an Text value as a shared secret.

Here we'll the see how to use conferer's `Generics` mechanism to automatically read configuration
values.

### Creating the config

In this tutorial we'll use:

* [conferer](https://hackage.haskell.org/package/conferer): core lib
* [conferer-warp](https://hackage.haskell.org/package/conferer-warp): parsing warp config
* [conferer-hedis](https://hackage.haskell.org/package/conferer-hedis): parsing hedis config

First we setup the imports:

```haskell
import qualified Conferer
import Conferer.FromConfig.Warp ()
import Conferer.FromConfig.Hedis ()
```

### Our custom configuration record

For this example we know what we want to configure so let's create a record to hold that data:

```haskell
data AppConfig = AppConfig
  { appConfigWarp :: Warp.Settings
  , appConfigRedis :: Hedis.ConnectionInfo
  , appConfigSecret :: Text
  } deriving (Show)
```

### Getting our record from the config

So there are two important typeclasses here:


### `DefaultConfig`

This instance defines the default value for your record, one the main principles for conferer is
that the program should be able to run even if no config values are passed in.

So most values that come from other libraries have defaults, but we have a `Text` as well, so what's
a good default for _any_ `Text`? We decided there is none. In most cases a primitive type's default
is defined by the record that holds it, so that's what we'll do, provide a default for
`appConfigSecret` via the record that holds it.

```haskell
instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigWarp = Conferer.configDef
    -- { appConfigWarp = setPort 2222 configDef
    , appConfigRedis = Conferer.configDef
    , appConfigSecret = "very secret... shhh"
    }
```

Note: this typeclass is never used internally but it's useful for users since most libraries'
defaults are not as easy to find as they could be.


### `FromConfig`

This instance defines how it uses a `config` and return a value. This instance is the where
most of the magic happens, luckily in most cases we can get pretty far using `Generics` to
implement this typeclass automatically.

To do that we first import `GHC.Generics`

```haskell
import GHC.Generics
```

Then we need to derive the `Generic` typeclass for our type, and to do that we need to add a compiler
extension `DeriveGeneric`.

So atop our file, before imports, we add the extension:

```haskell
{-# LANGUAGE DeriveGeneric #-}
```

Then just like `Show` we add the `Generic` typeclass to our `deriving`.

```haskell
data AppConfig = AppConfig
  { -- Elided
  } deriving (Show, Generic)
```

And to wrap it all up we tell GHC to use the default implementation for `FromConfig` which is `Generics`-based

```haskell
instance FromConfig AppConfig
```

### Wiring it up to our application

Just like we did in the [first tutorial](/docs/tutorial) we finish up by getting our value from a config.

```haskell
main = do
  config <- Conferer.mkConfig "awesomeapp"
  appConfig <- Conferer.fetch config :: IO AppConfig
  doTheThing appConfig
```

Note: The generics based `FromConfig` implementation follows a common practice of using prefixed names
for your records, so for example we have `AppConfig` which has a `secret`, its field is named `appConfigSecret`.
Conferer knows this so while generating the `FromConfig` code it checks that the constructor is a prefix of the
field name and strips it away in the configuration key name, so `appConfigSecret` inside `AppConfig` turns into
`secret`. If the constructor is not a prefix then the field name is used as is.

In the future I'd like to make this behavior configurable but for now I think it's a good default.

### What we've got

With that code we get the same as before, we can configure the same things in multiple ways, for
example for settings the key `"server.port"` we can use:

* env vars: `AWESOMEAPP_SERVER_PORT=5555`
* cli params: `--server.port=5555`
* properties file: `./config/development.properties` with `server.port=5555`

And we can set a bunch of values from or appConfig:

* `server.port=5555`: set warp's listening port to 5555
* `secret=real_secrets`: set our custom secret to `"real_secrets"`
* `hedis=redis://username:password@host:42/2`: set hedis' connection string directly
* `hedis.host=redis.example.com`: set hedis' connection host to `redis.example.com` and use default
values for everything else

That's it. if you want to know more about conferer you can check out the docs for [core concepts](/docs/core-concepts),
also for the specifics of [conferer-warp](/docs/fromConfig/warp) and [conferer-warp](/docs/fromConfig/hedis).

### Final Code

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.Redis as Hedis

import qualified Conferer
import Conferer.FromConfig.Warp ()
import Conferer.FromConfig.Hedis ()

data AppConfig = AppConfig
  { appConfigWarp :: Warp.Settings
  , appConfigRedis :: Hedis.ConnectionInfo
  , appConfigSecret :: Text
  } deriving (Show, Generic)

instance FromConfig AppConfig
instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigWarp = configDef
    , appConfigRedis = configDef
    , appConfigSecret = "very secret... shhh"
    }

main = do
  config <- Conferer.mkConfig "awesomeapp"
  appConfig <- Conferer.fetch config :: IO AppConfig
  doTheThing appConfig
```
