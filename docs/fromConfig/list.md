---
id: list
title: List FromConfig
---

## List FromConfig

This FromConfig instance provides support for parsing haskell's lists.

Parsing a configuration as a list will use the keys to sort the values lexicographically and it will parse each value using the FromComfig instance associated to the type of the elements the list contains.

```
allowed_ips.01=101.125.246.105
allowed_ips.02=7.173.68.54
```
fetching a list of String from the following config using the key `"allowed_ips"` would return `["101.125.246.105", "7.173.68.54"]`

---

```
allowed_ips.ludat=7.173.68.54
allowed_ips.juan=101.125.246.105
```
fetching a list of String from the following config using the key `"allowed_ips"` would return `["101.125.246.105", "7.173.68.54"]`.

### Special Keys

#### keys

If you don't want to use the default lexicographic order, you can add a `keys` key to the config with a comma separated list consisting of the other keys in the config. The order in the parsed list will be the same as the order in that list.

 ```
 allowed_emails.ludat=ludat@aol.com
 allowed_emails.juan=juan@aol.com
 allowed_emails.keys=ludat,juan
 ```
 fetching a list of String from that config using the key "allowed_emails" would return `["ludat@aol.com", "juan@aol.com"]`.

---

The value of `keys` does not need to have all other keys in the configuration. If some key is missing then it won't appear in the parsed list.

```
allowed_emails.ludat=ludat@aol.com
allowed_emails.juan=juan@aol.com
allowed_emails.keys=ludat
```
fetching a list of String from the following config using the key "allowed_emails" would return `["ludat@aol.com"]`

---

Specifying keys that are not present in the configuration will result in an error.

```
allowed_emails.ludat=ludat@aol.com
allowed_emails.juan=juan@aol.com
allowed_emails.keys=facu@aol.com
```
fetching a list of String from that config using the key "allowed_emails" would raise an error saying that the key facu was not found.

---

Finally, you can reference the defaults in `keys` by index like `defaults.0`, `defaults.1`, etc.

```
allowed_emails.ludat=ludat@aol.com
allowed_emails.juan=juan@aol.com
allowed_emails.keys=ludat,juan,defaults.0
```
if we had a default list of emails configured as `["facu@aol.com"]`, we could fetch a list of String from the config using the key "allowed_emails" and get `["ludat@aol.com", "juan@aol.com", "facu@aol.com"]`

#### prototype

Sometimes you might have more complex objects in your keys that need to have their own subkeys. For example, if we defined an Email type like this one:

```
data Email = Email { username :: String, domain :: String } deriving (Generic, Show)

instance FromConfig Email
```

Then the config that we had in the last examples won't work, we'll need to rewrite this:
```
allowed_emails.ludat=ludat@aol.com
allowed_emails.juan=juan@aol.com
```
into:
```
allowed_emails.ludat.username=ludat
allowed_emails.ludat.domain=aol.com
allowed_emails.juan.username=juan
allowed_emails.juan.domain=aol.com
```

And when we fetch a list of `Email` using the key `"allowed_emails"` we will get `[Email "juan" "aol.com", Email "ludat" "aol.com"]`.

--- 

Also, let's say that in this app most allowed emails share the same domain. In order to avoid repeating it every time, we can use the key `prototype` like this:
```
allowed_emails.ludat.username=ludat
allowed_emails.juan.username=juan
allowed_emails.prototype.domain=aol.com
```

That config will yield the same list of emails than the one before. The prototype's `domain` value is used for all keys that do not have a `domain` set explicitly.

---

If there's a key that already has a `domain` set, that one will be used:
```
allowed_emails.ludat.username=ludat
allowed_emails.juan.username=juan
allowed_emails.facu.username=facu
allowed_emails.facu.domain=yahoo.com
allowed_emails.prototype.domain=aol.com
```
that config results in `[Email "facu" "yahoo.com", Email "juan" "aol.com", Email "ludat" "aol.com"]`.

---

It's not the most usual thing to do, but if a prototype fully defines a value, it will be used for any unknown key:

```
allowed_emails.ludat.username=ludat
allowed_emails.juan.username=juan
allowed_emails.prototype.username=example
allowed_emails.prototype.domain=aol.com
allowed_emails.keys=facu,juan,ludat,joaco
```
that config results in `[Email "example" "yahoo.com", Email "juan" "aol.com", Email "ludat" "aol.com", Email "example" "yahoo.com"]`.
