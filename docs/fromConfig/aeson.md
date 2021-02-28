---
id: aeson
title: Aeson (JSON) FromConfig
---

## Aeson FromConfig

This FromConfig instance provides support for parsing values of type `Data.Aeson.Value`.

This instance **does not** traverse keys at all, it only reads a the very same key and
tries to parse it as JSON.

## Examples

With the config

```properties
some.json=[1,2,3]
```

`fetchFromConfig @Aeson.Value "some.json" config` results in: `[1,2,3]`.

---

With the config

```properties
some.json.0=1
some.json.1=2
some.json.2=3
```

`fetchFromConfig @Aeson.Value "some.json" config` results in a missing key error.

---

With the config

```properties
some.json=something
```

`fetchFromConfig @Aeson.Value "some.json" config` results in a failed parse error since
`something` is not valid json (the quotes must be present).

Some sources are treat quotes in different ways so for example:

* `APP_SOME_JSON=\"something\"` for env vars
* `--some.json=\"something\"` for cli args
* `{"some": {"json": "\"something\""}}` for json source
* `some.key="something"` for properties source
