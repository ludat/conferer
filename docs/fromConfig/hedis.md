---
id: hedis
title: Hedis FromConfig
---

## Hedis FromConfig

This FromConfig instance provides support for parsing [Hedis's ConnectInfo](https://hackage.haskell.org/package/hedis-0.13.1/docs/Database-Redis.html#t:ConnectInfo).

## Url and overrides

This instance can read redis urls, but if any more specific value is given that value will
be used instead.

So if you have `REDIS_PORT=1000` and cli arg `--redis=redis://host:7000`, the port will be `1000` since
`"redis.port"` is more specific than `"redis"`. This may change in the future but that's the current
behavior, if enough people find this confusing I may change it.

## Configurable values

### `host :: HostName`

Hostname is a String that tells hedis where the redis server is located.

Default: `"localhost"`

### `port :: PortID`

PortID is either a port number or a path to a unix socket that connects to redis, this fromConfig
tries to parse a number and if it fail it assumes the value is a unix socket. The unix socket
functionality is only available on unix systems.

Default: `PortNumber 6379`

### `auth :: Maybe ByteString`

When the server is protected by a password. Each connection will then authenticate by
the auth command.

Default: `Nothing`

### `database :: Integer`

Each connection will select the database with the given index.

Default: `0`

### `maxConnections :: Int`

Maximum number of connections to keep open. The smallest acceptable value is 1.

Default: `50`

### `maxIdleTime :: NominalDiffTime` (NOT CONFIGURABLE BY USER YET)

Amount of time for which an unused connection is kept open. The smallest acceptable value is 0.5 seconds. If the timeout value in your redis.conf file is non-zero, it should be larger than connectMaxIdleTime.

Default: `30`

### `timeout :: Maybe NominalDiffTime` (NOT CONFIGURABLE BY USER YET)

Optional timeout until connection to Redis gets established. ConnectTimeoutException gets thrown if no socket get connected in this interval of time.

Default: `Nothing`

### `tlsParams :: Maybe ClientParams` (NOT CONFIGURABLE BY USER YET)

Optional TLS parameters. TLS will be enabled if this is provided.