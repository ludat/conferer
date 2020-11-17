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
  --   snapConfig <- 'getFromConfig' \"snap\" config
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

-- instance (Snap.MonadSnap m) => DefaultConfig (Snap.Config m a) where
--   configDef = Snap.defaultConfig

instance forall a m. (FromConfig a, Typeable a, Snap.MonadSnap m, Typeable m) => FromConfig (Snap.Config m a) where
  fetchFromConfig key config = do
    Snap.Config{..} <- fetchFromDefaults key config
    defaultTimeout <- getFromConfigWithDefault (key /. "defaultTimeout") config defaultTimeout
    accessLog <- getFromConfigWithDefault (key /. "accessLog") config accessLog
    bind <- getFromConfigWithDefault (key /. "bind") config bind
    compression <- getFromConfigWithDefault (key /. "compression") config compression
    errorLog <- getFromConfigWithDefault (key /. "errorLog") config errorLog
    hostname <- getFromConfigWithDefault (key /. "hostname") config hostname
    locale <- getFromConfigWithDefault (key /. "locale") config locale
    port <- getFromConfigWithDefault (key /. "port") config port
    proxyType <- getFromConfigWithDefault (key /. "proxyType") config proxyType
    sslbind <- getFromConfigWithDefault (key /. "sslBind") config sslbind
    sslcert <- getFromConfigWithDefault (key /. "sslCert") config sslcert
    sslkey <- getFromConfigWithDefault (key /. "sslKey") config sslkey
    sslchaincert <- getFromConfigWithDefault (key /. "sslChainCert") config sslchaincert
    sslport <- getFromConfigWithDefault (key /. "sslPort") config sslport
    verbose <- getFromConfigWithDefault (key /. "verbose") config verbose
    unixsocket <- getFromConfigWithDefault (key /. "unixSocket") config unixsocket
    unixaccessmode <- getFromConfigWithDefault (key /. "unixSocketAccessMode") config unixaccessmode
    other <- getFromConfigWithDefault @(Maybe a) (key /. "other") config other

    pure Snap.Config{..}
