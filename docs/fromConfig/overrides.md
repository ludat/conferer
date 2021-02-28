---
id: overrides
title: Overriding any FromConfig
---

## Override any `FromConfig` instance

This is the ultimate scape hatch, it allows overriding any `FromConfig` instances
even ones defined inside an already defined `FromConfig` instance.

## A motivating example

Let's say we have a program that uses warp and we want to change slightly the
way we get the port (we might want semantic names like `"http"` which would
mean port `80`).

That instance makes sense for the `Int` inside `warp` but not for `Int` in
general so we can use the override mechanisms.

So when we create the `Config` we use `mkConfig'` instead of `mkConfig`

```haskell
betterConfig = someConfig 
    & addDefault "server.port" (overrideFetch @Int theNewFetchFunction)
```

That means that when something fetched an `Int` from `"server.port"` it will use
`theNewFetchFunction`.

## Technical details

This is allowed by `fetchFromConfig` which inspects the defaults for a value of
type `OverrideFromConfig a`, if it's present it'll use that instead of the
instance for `a`.

## Warning

This may possibly change in the future since it's so tightly coupled with the
inner workings of the library but we'll try to maintain the compatibility.