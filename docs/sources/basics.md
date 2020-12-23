---
id: basics
title: Basic Sources
---

## Command line argument source

This source allows conferer to read configuration from the CLI aguments:

It maps arguments starting like `--key.sub=value` into key `"key.sub"` with value `"value"`

it ignored anything that doesn't have a `--` at the beginning and if the `=` is not present
then the value is `"true"` (as the usual convention on for CLIs)

## Environment source

This source allows conferer to read configuration from environment variables:

It maps keys to a common format for env vars by:

* Uppercase everything (`server.port` => `SERVER.PORT`)
* Join key into a big string using underscores (`"SERVER.PORT"` => `"SERVER_PORT"`)

NOTE: The default config combines this Source so env vars are scoped using the
application name so `server.port` => `AWESOMEAPP_SERVER_PORT`.

## Properties file source

This source allows conferer to read configuration from .properties file:

NOTE: This source is quite rigid now, in the future when we see how users use this module we'll
expand it

This module is configured recursively, it uses the `"env"` key to find out which file to use
like `config/${env}.properties`, so if it's set to production the file will be
`config/production.properties`. If no `env` key is set, `"development"` will be used.

The User (or The Programmer) can also provide a `Key` to look for a filepath pointing to the
configuration file, so setting `config.file=somefile.properties` will use that file ignoring
the `"env"` key.

The format of the file is as follows:

```
key.sub=value
other.key.sub=value

some.key= 74

```

There are no comments and spaces are not trimmed so `"some.key"`'s value will be `" 74"` instead 
of `"74"`. Blank lines are ignored. (May be configurable in the future)

This format is pretty restricted, I plan to improve it further in later versions but for now it's
enough and there are other more standard formats that are capable of representing everything as
well.
