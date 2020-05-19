---
id: getting-started
title: Getting Started
sidebar_label: Getting started
---

## The problem

Have you ever tried configuring a Haskell application? If you are not the author
you are usually out of luck and the only way to configure it is recompiling, and
even if you are the author you have to write that logic yourself (reading env vars,
files or cli params), what about partial values? and defaults? and environments?
or error handling?

## One solution: Conferer

Conferer is a library that defines ways of getting configuration for your
Haskell application and the libraries it uses in a very ergonomical way.

## Features

### Simple and lightweight

Conferer is designed to be as simple as possible but no more. Browsing the source code
shouldn't throw a beginner into a spiral of doom. As such, the core library has very 
few dependencies (only ghc-bundled dependencies) since most dependencies reside in the 
project specific packages like `warp`, `hspec`, etc.

### Low Boilerplate

By leveraging [Generics](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html),
we get a lot for free without having to write more code than the strictly necessary.

### Good defaults without compromising extensibility

By default, the library is usable without writing too much code but many point of 
extension are set in place to ease the adding of new configuration values and 
sources.

### Fail fast <!-- and debuggable -->

Wherever the library finds something unexpected it throws an exception with a clear 
error message<!--  and since we are interacting with the real world debugging it is easy  -->
<!-- by using the debug mode -->.
