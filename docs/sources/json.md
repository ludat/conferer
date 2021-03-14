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

* `"some"` not present
* `"some.key"` mapped to `"value"`
* `"other"` mapped to `"true"`
* `"list.0"` mapped to `"1"`
* `"list.1"` mapped to `"2"`
* `"list.3.k"` mapped to `"x"`

Note that:

* Since conferer's values always are string, the `true` becomes `"true"`, conferer's
    FromConfig knows how to interpret this properly though (same as numbers and null).
* We traverse the tree until we find a primitive value.
* We don't define keys for nested values like `"some"` in this case.
* For list we define a key with its index and they can nest other values.
* The json file may only contain valid keys (lowercase ascii and numbers)
  otherwise an exception will be thrown.

### Special `_self` key

Conferer allows setting some key and at the same time more specific keys at the same
time.

For example setting:

`somefile`=myfile.png
`somefile.extension`=jpg

is possible and when parsing `File` this will result in `myfile.jpg` (more specific wins)

Json cannot do that natively so we have the `_self` special key, which is interpreted as
setting the value of the object while still allowing more specific keys to be set.

The example above in json would be:

```json
{
    "somefile": {
        "_self": "myfile.png",
        "extension": "jpg"
    }
}
```

### Special `keys` key

For nested values the magic `keys` key is present and has a comma separated list
of the present keys


```json
{
    "list": [3,4,5],
    "object": {"a": 1, "b": 7}
}
```

exposes

* `"list"` not present
* `"list.0"` mapped to `"3"`
* `"list.keys"` mapped to `"0,1,2"`
* `"object"` not present
* `"object.a"` mapped to `"1"`
* `"object.keys"` mapped to `"a,b"`

NOTE: The keys list is always sorted in lexicographical order, since aeson doesn't
maintain the original order of keys, we sort it to be consistent.
