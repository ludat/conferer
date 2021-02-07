---
id: list
title: List FromConfig
---

## List FromConfig

This FromConfig instance provides support for parsing haskell's lists.

Parsing a configuration as a list will use the keys to sort the values lexicographically and it will parse each value using the FromComfig instance associated to the type of the elements the list contains.

### 'Keys' special key

If you don't want to use the default lexicographic order, you can add a `keys` key to the config with a comma separated list consisting of the other keys in the config. The order in the parsed list will be the same as the order in that list.

The value of `keys` does not need to have all other keys in the configuration. If some key is missing then it won't appear in the parsed list. Specifying keys that are not present in the configuration will result in an error.

Finally, you can reference the defaults in `keys` by index like `defaults.0`, `defaults.1`, etc.
