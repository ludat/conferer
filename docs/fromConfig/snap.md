---
id: snap
title: Snap FromConfig
---

## Snap FromConfig

This FromConfig instance provider support for parsing [Snap's Config](https://hackage.haskell.org/package/snap-server-1.1.1.2/docs/Snap-Internal-Http-Server-Config.html#t:Config)

## Configurable values

### `hostname :: Maybe ByteString`

Default hostname to use if a request comes without a
"Host" header

Default: `Just "localhost"`

### `accessLog :: Maybe ConfigLog`

Path to access log

Default: `Just "log/access.log"`

### `errorLog :: Maybe ConfigLog`

Path to error log

Default: `Just "log/error.log"`

### `locale :: Maybe String`

Default locale to use to set the LANG,LC_ALL,etc. to that value

Default: `Just "en_US"`

### `port :: Maybe Int`

Port to listen on, Nothing means 8000

Default: `Nothing`

### `bind :: Maybe ByteString`

Address to bind to.

Default: `Just "0.0.0.0"`

### `sslport :: Maybe Int`

Port to listen on ssl.

Default: `Nothing`

### `sslbind :: Maybe ByteString`

Address to bind to for ssl.

Default: `Nothing`

### `sslcert :: Maybe FilePath`

Path to the cert file path.

Default: `Nothing`

### `sslchaincert :: Maybe Bool`

Path to the SSL certificate file.

Default: `Nothing`

### `sslkey :: Maybe FilePath`

Path to the SSL key file.

Default: `Nothing`

### `unixsocket :: Maybe FilePath`

File path to unix socket. Must be absolute path, but allows for symbolic
links.

Default: `Nothing`

### `unixaccessmode :: Maybe Int`

Access mode for unix socket, by default is system specific.

Default: `Nothing`

### `compression :: Maybe Bool`

If set and set to True, compression is turned on when applicable.

Default: `Nothing`

### `verbose :: Maybe Bool`

Whether to write server status updates to stderr.

Default: `Nothing`

### `errorHandler :: Maybe (SomeException -> m ())` (NOT CONFIGURABLE BY USER YET)

Run some callback when there is an error on a handler

Default: `Nothing`

### `defaultTimeout :: Maybe Int`

Timeout for incoming requests.

Default: `Nothing`

### `other :: Maybe a`

Custom user defined configuration

Default: `Nothing`

### `proxyType :: Maybe ProxyType`

Sets the behavior for the X-Forwarded header, there are three options:

* `none`: Ignores the header
* `haproxy`: Acts as if it was behind haproxy using the haproxy protocol
* `x_forwarded_for`: Acts as if snap is behind an http reverse proxy

Default: `Nothing`

### `startupHook :: Maybe (StartupInfo m a -> IO ()` (NOT CONFIGURABLE BY USER YET)

Execute some code before the server starts

Default: `Nothing`

