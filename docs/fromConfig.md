---
id: from-config
title: FromConfig
---

Before reading this doc, read the [core concepts](/docs/core-concepts).

## Why would I want to read this doc?

You don't need to read this doc to use conferer, you only need this if you
plan to write a custom `FromConfig` instance for some type, otherwise
this will probably won't be of much help.

## Who?

Throughout this doc I'll reference two actors:

* The Programmer: This is the person writing haskell that has direct access
    to the source code.
* The User: This is the person using the haskell program, this person doesn't
    have access to the source (or maybe recompiling would be too hard) but is
    the one executing the haskell program made by The programmer.

As usual The Programmer and The User may be the same person.

## Where is FromConfig?

The module `Conferer.FromConfig` is the stable interface for `FromConfig`. If you
really need it you can import `Conferer.FromConfig.Internal` but that could break
without notice so be careful.

## What is FromConfig?

The point of conferer is to provide configuration values for most libraries
and to do this we use a `Config` and a `Key`.

A `Key` is a simple object (mostly a `[String]`) which is useful to reference
things inside a `Config`. In turn a `Config` is an object that we can query
for value associated with some `Key`, this value is either a `Text` or a
[`Dynamic`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Dynamic.html).

The `Dynamic` values are called "defaults", which are defined only by The Programmer
and are used to customize the behavior of `FromConfig` instances so the instance
fits The Programmer needs and in turn The User's expectations.

The `Text` values are defined by The User, via cli args, env var, etc. (we'll get
here when we discuss the `Source`s), they usually take priority over "defaults",
so The User can override most things by default.

## Some conventions

### MissingKeyError

This exception is slightly special since it's used internally and should be thrown
whenever a `FromConfig` instance needs a `Key` that's not present.

### Defaults should be respected

A `FromConfig` instance should use the default that appears on the key that is was
asked to parse, so this should hold for any `FromConfig`

```haskell
spec = do 
  fetchedValue <-
    fetchFromConfig key (config & addDefault key someTypeDefault)
  fetchedValue `shouldBe` someTypeDefault
```

### If something looks weird fail fast

Most of the time a default will be present so you could ignore bad input but most
of the time you should tell The User or The Programmer that they provided bad input
(either bad `Text` or `Dynamic`).

So if you expect an `Int` and you see the `Text` "abc", you should throw
Mirroring this, if you expect an `Int` and you find a `Dynamic` which has a `Text`
you should throw.

Note that this last error won't be easily fixable by The User so you should let
The Programmer using your instance to look out for this.
