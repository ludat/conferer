---
id: warp
title: Warp FromConfig
---

## Warp FromConfig

This FromConfig instance provider support for parsing [Warp's Settings](https://hackage.haskell.org/package/warp-3.3.11/docs/Network-Wai-Handler-Warp.html#t:Settings).
It supports versions from 2.0.0 up to the latest version (currently 3.3.13).

## Configurable values

### `port :: Integer`

Port to listen on. Default value: 3000

### `host :: HostPreference`

Which host to bind.

| Magic value | Meaning                                   |
|-------------|-------------------------------------------|
| `"*"`       | any IPv4 or IPv6 hostname                 |
| `"*4"`      | any IPv4 or IPv6 hostname, IPv4 preferred |
| `"!4"`      | any IPv4 hostname                         |
| `"*6"`      | any IPv4 or IPv6 hostname, IPv6 preferred |
| `"!6"`      | any IPv6 hostname                         |

Note that the permissive * values allow binding to an IPv4 or an IPv6 hostname, which 
means you might be able to successfully bind to a port more times than you expect 
(eg once on the IPv4 localhost 127.0.0.1 and again on the IPv6 localhost 0:0:0:0:0:0:0:1).

Any other value is treated as a hostname. As an example, to bind to the IPv4 local host only, use "127.0.0.1".

Default value: `"*4"` (any IPv4 or IPv6 hostname, IPv4 preferred)

### `timeout :: Int`

Timeout value in seconds.

Default value: 30

### `fdCacheDuration :: Int`

Cache duration time of file descriptors in seconds. 

0 means that the cache mechanism is not used.

Default value: 0

### `fileInfoCacheDuration :: Int`

Cache duration time of file information in seconds. 

0 means that the cache mechanism is not used. Default value: 0

### `noParsePath :: Bool`

Perform no parsing on the rawPathInfo.

This is useful for writing HTTP proxies.

Default: False

Since 2.0.3

### `serverName :: ByteString`

Default server name if application does not set one.

Since 3.0.2

### `maximumBodyFlush :: Maybe Int`

The maximum number of bytes to flush from an unconsumed request body in bytes.

By default, Warp does not flush the request body so that, if a large body is present, the connection 
is simply terminated instead of wasting time and bandwidth on transmitting it. However, some clients do 
not deal with that situation well. You can either change this setting to Nothing to flush the entire 
body in all cases, or in your application ensure that you always consume the entire request body.

Default: 8192 bytes.

Since 3.0.3

### `proxyProtocol :: ProxyProtocol`

Specify usage of the PROXY protocol. There values are available

* `"ProxyProtocolNone"`:

Do not use the PROXY protocol.

* `"ProxyProtocolRequired"`

Require PROXY header.

This is for cases where a "dumb" TCP/SSL proxy is being used, which cannot add an X-Forwarded-For HTTP header field but has enabled support for the PROXY protocol.

See http://www.haproxy.org/download/1.5/doc/proxy-protocol.txt and http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#proxy-protocol.

Only the human-readable header format (version 1) is supported. The binary header format (version 2) is not supported.

* `"ProxyProtocolOptional"`

Use the PROXY header if it exists, but also accept connections without the header. See setProxyProtocolRequired.

WARNING: This is contrary to the PROXY protocol specification and using it can indicate a security problem with your architecture if the web server is directly accessible to the public, since it would allow easy IP address spoofing. However, it can be useful in some cases, such as if a load balancer health check uses regular HTTP without the PROXY header, but proxied connections do include the PROXY header.

Since 3.0.5

### `slowlorisSize :: Int`

Size of bytes read to prevent Slowloris protection.

Default value: 2048

Since 3.1.2

### `http2Enabled :: Bool`

Whether to enable HTTP2 ALPN/upgrades.

Default: True

Since 3.1.7

### `gracefulShutdownTimeout :: Maybe Int`

An optional timeout to limit the time (in seconds) waiting for a graceful shutdown of the web server.

Since 3.2.8

### `gracefulCloseTimeout1 :: Int`

A timeout to limit the time (in milliseconds) waiting for
FIN for HTTP/1.x. 0 means uses immediate close.
Default: 0.

Since 3.3.5

### `gracefulCloseTimeout2 :: Int`

A timeout to limit the time (in milliseconds) waiting for
FIN for HTTP/2. 0 means uses immediate close.

Default: 2000.

Since 3.3.5

### `maxTotalHeaderLength :: Int`

Determines the maximum header size that Warp will tolerate when using HTTP/1.x.

Since 3.3.8

### `altSvc :: Maybe ByteString`

Specify the header value of Alternative Services (AltSvc:).

Default: Nothing

Since 3.3.11

## Not user configurable

There are many configuration values that are not configurable through conferer since they are not
serializable (functions for example), but you can configure those using either `addDefault` or
passing a default value that has these properly configured.

These include:

* onException
* onExceptionResponse
* onOpen
* onClose
* beforeMainLoop 
* fork
* installShutdownHandler
* logger
* serverPushLogger

You can set the default values by creating your own record (</docs/01-tutorial.md>).
