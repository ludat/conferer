---
id: core-concepts
title: Core concepts
---

## The interface between two worlds

There are two important parts of this library:

* sources: which define the way you get configuration values
* fromConfig: that define how to interpret configuration values to create some settings
    object for certain library

Now the crucial part is what brings them together: a map from keys to strings, here a key could be
`"server.port"`, and its value `"8000"`. `Key`s may be defined at any level so having both
`"key"` and `"key.sub"` at the same time, it's up to the fromConfig instance to interpret.

So those are a lot of words, lets put examples in there:

* **Key**: This a hierarchical String list, which is represented as a dot separated value (e.g.
`"server.port"`). They are used by **Sources** and implement `IsString` so they look like `String`s
* **Sources**: This object could be abstracted as a `Key -> Maybe Text`. Its job is to get
values from any source that and make it match the key to string interface.
* **Config**: This is very similar to **Source** , but with a couple added things like defaults and
its usually made of many **Sources**. It's the object that the user will interact with directly.
* **FromConfig**: This is a typeclass that gives semantic to the Strings from **Sources** and turns
them into some useful Haskell value (e.g. warp's `Settings`)
* **DefaultConfig**: A program should work without configuration so this typeclass defines the value
that we get if we don't define anything, and what gets partially updated if the config is not
complete

So there are two extension points that need not to know about each other: Sources and fromConfig,
so if tomorrow I feel like reading configs from a json file, warp's FromConfig instance doesn't
case, and the same is true the other way around, if I want to start fetching mysql config then
the env vars doesn't need to know about it.

More documentation on [fromConfig](/docs/from-config) and [sources](/docs/sources)

