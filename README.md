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

## what, why and a bit of how

Conferer is a library that defines ways of getting configuration for your
Haskell application and the libraries it uses, which is at heart string keys
which map to other strings.

To get this map we use `Providers` which define a way to get a key (eg.
`db.username`) that may or may not exist, we then use a list of providers for
getting the value for that key (position on the list defines priority). This
allows adding new providers easily (for example a dhall file provider,
a git repo or a etcd database)

The other side of this is that we have the `FromConfig` which gets some value
from a `Config` at a certain key possibly using only keys under some namespacing
key

## Example

Let's say I want to configure warp. Let's say we wrote this program.

```haskell
main = do
  -- by default gets cli parameters, envvars and properties file
  config <- getDefaultConfigFor "awesomeapp"
  warpConfig :: Warp.Settings <- getKey "warp" config

  Warp.runSettings warpConfig myApp
```

Now I need to chage the port of the app, I can change it by either:

* Setting cli params like `./myApp --warp.port=5555`
* Setting an environment variable called `AWESOMEAPP_WARP_PORT=5555`
* In a `config/dev.properties` file, you can have `warp.port=5555`

And you may also get that value from different configuration providers like
redis, json file, dhall file or whichever you may need.

## Existing providers

Providers usually incur in many dependencies so they are split into different
packages

* *Json files*: You can use json files to configure your app (depends on
  `aeson`)
* *Dhall files*: You can use dhall files to configure your app (depends on
  `dhall`)
* *Yaml files*: You can use Yaml files to configure your app (depends on `yaml`)

## Existing FromConfig instances

Default instances for fetching a values from a config (usually a config value
for some library)

* *snap-server*: You can fetch a config value for the snap library (which
  configures how the server runs)
* *warp*: You can fetch a config value for the warp library
* *hspec*: You can fetch a config value for the hspec library

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
