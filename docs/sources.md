---
id: sources
title: Sources
---

Before reading this doc, read the [core concepts](/docs/core-concepts) and
[fromConfig](/docs/fromConfig) page.

## Why would I want to read this doc?

You don't need to read this doc to use conferer, you only need this if you
plan to write a custom `Source` otherwise this will probably won't be of
much help.

## Who?

Throughout this doc I'll reference two actors:

* The Programmer: This is the person writing haskell that has direct access
    to the source code.
* The User: This is the person using the haskell program, this person doesn't
    have access to the source (or maybe recompiling would be too hard) but is
    the one executing the haskell program made by The programmer.

As usual The Programmer and The User may be the same person.

## Where is a source?

The module `Conferer.Source` is the stable interface for `Source`. If you
really need it you can import `Conferer.Source.Internal` but that could break
without notice so be careful.


## What is a source?

A `Source` provides a `FromConfig` instance with `Text` values, which can
be configured by The User.

A `Config` is mostly made of many `Source`s working together to provide
many extension points for `FromConfig` instances.

A `Source` knows how to map a `Key` into some other thing which The User
will be able to change and tell the `Config` if it's present or not.

## Implementing a source

The simplest source is the `Null` source (`Conferer.Source.Null`), which has
no keys, but explains the interface, a `Source` may use the `Config` object
to configure themselves (for example: to get the path of a file or the host
of a db), and the `SourceCreator` encapsulates that. To add a `Source` to a
`Config` you need to use the `SourceCreator` for that `Source` so it's a good
idea to provide one.

## Listing subkeys

Sources can also be asked to list present subkeys. These functionality is used
by more dyamic `FromConfig` like `List`, but it should return a list of `Key`s
that when queried should be present.
