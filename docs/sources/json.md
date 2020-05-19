---
id: json
title: Json source
---

## Json source

This source allows conferer to read configuration from json files.

This uses the same facilities as the properties file to resolve the file name so it uses the
`"env"` key to find out which file to use like `config/${env}.json`, so if it's set to
production the file will be `config/production.json`. If no `env` key is set,
`"development"` will be used.

There is a bit of interpretation since conferer's values are not a one to one mapping to 
json values.

```json

{
    "some": {
        "key": "value"
    },
    "other": true,
    "list": [1, 2, {"k": "x"}]
}
```

Now this json file will result in only two keys;

`"some.key"` mapped to `"value"`
`"other"` mapped to `"true"`
`"list.0"` mapped to `"1"`
`"list.1"` mapped to `"2"`
`"list.3.k"` mapped to `"x"`

Note that:

* Since conferer's values always are string the `true` becomes `"true"`, conferer's 
FromConfig knows how to interpret this properly though (same as numbers and null).
* We traverse the tree until we find a primitive value.
* We don't define keys for nested values like `"some"` in this case.
* For list we define a key with its index and they can nest other values


### The future (NOT IMPLEMENTED YET)

In the future I'd like to expose metadata here for example:

```json
{
    "list": [1]
}
```

exposes

`"list.0"` mapped to `"1"`
`"list.length"` mapped to `"1"`
