{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
module Conferer.FromConfig.Snap
  (
  -- * How to use this
  -- | FromConfig instance for snap server configuration
  --
  -- @
  -- import Conferer
  -- import Conferer.FromConfig.Snap ()
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   snapConfig <- 'fetchFromConfig' \"snap\" config
  -- @
  --
  -- * Internal utility functions
  -- | These may be useful for someone but are subject to change at any point so
  -- use with care
  ) where

import Conferer.FromConfig

import Data.Text (unpack, toLower)

import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Internal.Http.Server.Config as Snap
import qualified Snap.Core as Snap
import Data.Data (Typeable)
import Data.Dynamic (toDyn, Dynamic)

instance FromConfig Snap.ConfigLog where
  fetchFromConfig =
    fetchFromConfigWith $
      (\case
        "nolog" -> pure Snap.ConfigNoLog
        "none" -> pure Snap.ConfigNoLog
        "no" -> pure Snap.ConfigNoLog
        "false" -> pure Snap.ConfigNoLog
        t -> pure $ Snap.ConfigFileLog $ unpack t
      ) . toLower

instance FromConfig Snap.ProxyType where
  fetchFromConfig =
    fetchFromConfigWith $
      (\case
        "noproxy" -> pure Snap.NoProxy
        "none" -> pure Snap.NoProxy
        "false" -> pure Snap.NoProxy

        "haproxy" -> pure Snap.HaProxy
        "ha" -> pure Snap.HaProxy

        "xforwardedfor" -> pure Snap.X_Forwarded_For
        "forwarded" -> pure Snap.X_Forwarded_For
        "x-forwarded-for" -> pure Snap.X_Forwarded_For
        "x_forwarded_for" -> pure Snap.X_Forwarded_For
        _ -> Nothing
      ) . toLower

instance (Snap.MonadSnap m) => DefaultConfig (Snap.Config m a) where
  configDef = Snap.defaultConfig

desconstructSnapConfigToDefaults :: (Typeable a, Typeable m) => Snap.Config m a -> [(Key, Dynamic)]
desconstructSnapConfigToDefaults Snap.Config{..} =
  [ ("defaultTimeout", toDyn defaultTimeout)
  , ("accessLog", toDyn accessLog)
  , ("bind", toDyn bind)
  , ("compression", toDyn compression)
  , ("errorLog", toDyn errorLog)
  , ("hostname", toDyn hostname)
  , ("locale", toDyn locale)
  , ("port", toDyn port)
  , ("proxyType", toDyn proxyType)
  , ("sslBind", toDyn sslbind)
  , ("sslCert", toDyn sslcert)
  , ("sslKey", toDyn sslkey)
  , ("sslChainCert", toDyn sslchaincert)
  , ("sslPort", toDyn sslport)
  , ("verbose", toDyn verbose)
  , ("unixSocket", toDyn unixsocket)
  , ("unixSocketAccessMode", toDyn unixaccessmode)
  , ("errorHandler", toDyn errorHandler)
  , ("startupHook", toDyn startupHook)
  , ("other", toDyn other)
  ]

instance forall a m. (FromConfig a, Typeable a, Snap.MonadSnap m, Typeable m) => FromConfig (Snap.Config m a) where
  fetchFromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults
      (desconstructSnapConfigToDefaults :: Snap.Config m a -> [(Key, Dynamic)])
      key originalConfig
    defaultTimeout <- fetchFromConfig (key /. "defaultTimeout") config
    accessLog <- fetchFromConfig (key /. "accessLog") config
    bind <- fetchFromConfig (key /. "bind") config
    compression <- fetchFromConfig (key /. "compression") config
    errorLog <- fetchFromConfig (key /. "errorLog") config
    hostname <- fetchFromConfig (key /. "hostname") config
    locale <- fetchFromConfig (key /. "locale") config
    port <- fetchFromConfig (key /. "port") config
    proxyType <- fetchFromConfig (key /. "proxyType") config
    sslbind <- fetchFromConfig (key /. "sslBind") config
    sslcert <- fetchFromConfig (key /. "sslCert") config
    sslkey <- fetchFromConfig (key /. "sslKey") config
    sslchaincert <- fetchFromConfig (key /. "sslChainCert") config
    sslport <- fetchFromConfig (key /. "sslPort") config
    verbose <- fetchFromConfig (key /. "verbose") config
    unixsocket <- fetchFromConfig (key /. "unixSocket") config
    unixaccessmode <- fetchFromConfig (key /. "unixSocketAccessMode") config
    errorHandler <- fetchFromConfig (key /. "errorHandler") config
    startupHook <- fetchFromConfig (key /. "startupHook") config
    other <- fetchFromConfig @(Maybe a) (key /. "other") config
    pure Snap.Config{..}
