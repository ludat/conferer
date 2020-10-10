{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Conferer.Core
import Conferer.Types
import Conferer.FromConfig.Basics

import Data.Text (unpack)

import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Core as Snap

instance FromConfig Snap.ConfigLog where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig k config = do
    getKey k config
      >>= \case
        Just "NoLog" -> return $ Just $ Snap.ConfigNoLog
        Just t -> return $ Just $ Snap.ConfigFileLog $ unpack t
        Nothing -> return $ Nothing

instance (Snap.MonadSnap m) => DefaultConfig (Snap.Config m a) where
  configDef = Snap.defaultConfig

withMaybe :: (a -> c -> c) -> Maybe a -> c -> c
withMaybe f (Just a) c = f a c
withMaybe _f (Nothing) c = c

instance forall a m. (FromConfig a, Snap.MonadSnap m) => FromConfig (Snap.Config m a) where
  fetchFromConfig _k _config = return Nothing
  updateFromConfig k config snapConfig = do
    pure snapConfig
      >>= findKeyAndApplyConfig config k "defaultTimeout" Snap.getDefaultTimeout (withMaybe Snap.setDefaultTimeout)
      >>= findKeyAndApplyConfig config k "accessLog" Snap.getAccessLog (withMaybe Snap.setAccessLog)
      >>= findKeyAndApplyConfig config k "bind" Snap.getBind (withMaybe Snap.setBind)
      >>= findKeyAndApplyConfig config k "compression" Snap.getCompression (withMaybe Snap.setCompression)
      >>= findKeyAndApplyConfig config k "errorLog" Snap.getErrorLog (withMaybe Snap.setErrorLog)
      >>= findKeyAndApplyConfig config k "hostname" Snap.getHostname (withMaybe Snap.setHostname)
      >>= findKeyAndApplyConfig config k "locale" Snap.getLocale (withMaybe Snap.setLocale)
      >>= findKeyAndApplyConfig config k "port" Snap.getPort (withMaybe Snap.setPort)
      -- >>= findKeyAndApplyConfig config k "proxy-type" (withMaybe Snap.setProxyType)
      >>= findKeyAndApplyConfig config k "sslBind" Snap.getSSLBind (withMaybe Snap.setSSLBind)
      >>= findKeyAndApplyConfig config k "sslCert" Snap.getSSLCert (withMaybe Snap.setSSLCert)
      >>= findKeyAndApplyConfig config k "sslKey" Snap.getSSLKey (withMaybe Snap.setSSLKey)
      >>= findKeyAndApplyConfig config k "sslChain-cert" Snap.getSSLChainCert (withMaybe Snap.setSSLChainCert)
      >>= findKeyAndApplyConfig config k "sslPort" Snap.getSSLPort (withMaybe Snap.setSSLPort)
      >>= findKeyAndApplyConfig config k "verbose" Snap.getVerbose (withMaybe Snap.setVerbose)
      >>= findKeyAndApplyConfig config k "unixSocket" Snap.getUnixSocket (withMaybe Snap.setUnixSocket)
      >>= findKeyAndApplyConfig config k "unixSocketAccessMode" Snap.getUnixSocketAccessMode (withMaybe Snap.setUnixSocketAccessMode)
      >>= findKeyAndApplyConfig config k "other" Snap.getOther (withMaybe Snap.setOther)
      >>= return
