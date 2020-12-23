---
id: quick-tutorial
title: Quick tutorial
---

## Simple configuration

### Our Current app

You have a [warp](https://hackage.haskell.org/package/warp) application which serves
`myApp` like this:

```haskell
main = do
  Warp.run myApp
```

### Imports

Now we are here to configure things (hopefully without much work), so let's install
the dependencies we need:

First [conferer](https://hackage.haskell.org/package/conferer), which is the core library
which provides the basic functions and typeclasses. To import conferer

```haskell
import qualified Conferer
```

We also need to import [conferer-warp](https://hackage.haskell.org/package/conferer-warp)
which tells conferer how to create warp's `Settings`.

```haskell
import Conferer.FromConfig.Warp ()
```

Note: From conferer-warp we only need some instances hence the `()`

### Creating a config

Now we need to do two things first create a `Config`, this object tells everything else where to
look for configuration values. To create it call `mkConfig "yourappname"`, use the your app's
name there.

So our program now looks like this:

```haskell
main = do
  config <- Conferer.mkConfig "awesomeapp"
```

### Fetching values from the config

With our newly created config, **we can use it to get a value of some type**, in our example here
we want to get a `Warp.Settings` from it, and we need to provide a default in case the user
doesn't configure that.

To do that we do:

```haskell
main = do
  config <- Conferer.mkConfig "awesomeapp"
  warpSettings <- Conferer.fetch config :: IO Warp.Settings
```

One of the mantras of conferer is that a program should work even if the user doesn provide
configuration, so most of the time you want to provide a default.

Note: here we need to type this explicitly since `warpSettings` could be of many types that also can be fetched
from a config. Once we start using it Haskell will be able to infer the type.

### Letting Warp know about Conferer

Finally we have to tell `warp` to use our `warpSettings`, to do that we need to pass the Settings object
we got to warp using `Warp.runWithSettings` (instead of `Warp.run` which doesn't allow using
external configurations but is more convenient)

```haskell
main = do
  config <- Conferer.mkConfig "awesomeapp"
  warpSettings <- Conferer.fetch config :: IO Warp.Settings
  Warp.runSettings warpSettings myApp
```

<!-- You may be wondering, why a prefix key (`"server"` in this case)? Imagine you have a postgresql database as
well as a warp server, they both read a `port` key, but you probably don't want them to be the same so we
scope they deserialization logic up front with the prefix, so for this example we'd use `"db"` as prefix for
postgres and `"server"` for warp, the configuration key for postgres' port would be `postgres.port` which
won't clash with warp's which would be `server.port`, you can use an empty string as the prefix to avoid
scoping but it's not a good idea since you don't know how a configuration will evolve. -->

### The configurable result

Now if I want to change the port of the app, I can change it by either:

* Setting cli params like `./myApp --port=5555`
* Setting an environment variable called `AWESOMEAPP_PORT=5555`
* In a `config/development.properties` file, you can have `port=5555`

But also we get many configurations that we usually can not touch like `serverName` (`Server` header
that warp reports), `timeout` (maximum time in seconds for inactive clients), and 15 other warp
configurations. If you want to know the options for a package you can take a look at [its docs](/docs/fromConfig/warp)
