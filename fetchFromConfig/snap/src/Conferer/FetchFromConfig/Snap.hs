module Conferer.FetchFromConfig.Snap
  (
  -- * How to use this
  -- | FetchFromConfig instance for snap server configuration
  --
  -- @
  -- import Conferer
  -- import Conferer.FetchFromConfig.Snap ()
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
import Conferer.FetchFromConfig.Basics

import Data.Either (rights)
import Data.String (fromString)
import Data.Text (Text, unpack)

import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Core as Snap

instance FetchFromConfig Snap.ConfigLog where
  fetch k config = do
    getKey k config
      >>= \case
        Just "NoLog" -> return $ Just $ Snap.ConfigNoLog
        Just t -> return $ Just $ Snap.ConfigFileLog $ unpack t
        Nothing -> return $ Nothing

instance (Snap.MonadSnap m) => DefaultConfig (Snap.Config m a) where
  defaultConfig = Snap.defaultConfig

instance (FetchFromConfig a, Snap.MonadSnap m) => FetchFromConfig (Snap.Config m a) where
  fetch k config = do
    pure (defaultConfig)
      >>= findKeyAndApplyConfig config k "default-timeout" Snap.setDefaultTimeout
      >>= findKeyAndApplyConfig config k "access-log" Snap.setAccessLog
      >>= findKeyAndApplyConfig config k "bind" Snap.setBind
      >>= findKeyAndApplyConfig config k "compression" Snap.setCompression
      >>= findKeyAndApplyConfig config k "error-log" Snap.setErrorLog
      >>= findKeyAndApplyConfig config k "hostname" Snap.setHostname
      >>= findKeyAndApplyConfig config k "locale" Snap.setLocale
      >>= findKeyAndApplyConfig config k "other" Snap.setOther
      >>= findKeyAndApplyConfig config k "port" Snap.setPort
      -- >>= findKeyAndApplyConfig config k "proxy-type" Snap.setProxyType
      >>= findKeyAndApplyConfig config k "ssl-bind" Snap.setSSLBind
      >>= findKeyAndApplyConfig config k "ssl-cert" Snap.setSSLCert
      >>= findKeyAndApplyConfig config k "ssl-key" Snap.setSSLKey
      >>= findKeyAndApplyConfig config k "ssl-chain-cert" Snap.setSSLChainCert
      >>= findKeyAndApplyConfig config k "ssl-port" Snap.setSSLPort
      >>= findKeyAndApplyConfig config k "verbose" Snap.setVerbose
      >>= findKeyAndApplyConfig config k "unix-socket" Snap.setUnixSocket
      >>= findKeyAndApplyConfig config k "unix-socket-access-mode" Snap.setUnixSocketAccessMode
      >>= (return . return)
