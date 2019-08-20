{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Conferer.FetchFromConfig.Snap where

import Conferer.Core
import Conferer.Types
import Conferer.FetchFromConfig.Basics
import Data.Either (rights)
import Data.String (fromString)
import Data.Text (Text, unpack)

import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Core as Snap

instance FetchFromConfig (Snap.ConfigLog) where
  fetch k config = do
    getKey k config
      >>= \case
        Right "NoLog" -> return $ Right $ Snap.ConfigNoLog
        Right t -> return $ Right $ Snap.ConfigFileLog $ unpack t
        Left e -> return $ Left e




instance (FetchFromConfig a, Snap.MonadSnap m) => FetchFromConfig (Snap.Config m a) where
  fetch k config = do
    pure (Right Snap.defaultConfig)
      >>= findKeyAndApplyConfig config k "default-timeout" Snap.setDefaultTimeout
      -- >>= findKeyAndApplyConfig config k "access-log" Snap.setAccessLog
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


findKeyAndApplyConfig ::
  FetchFromConfig newvalue
  => Config -> Key -> Key
  -> (newvalue -> config -> config)
  -> Either Text config -> IO (Either Text config)
findKeyAndApplyConfig config k relativeKey f (Right customConfig) =
  fetch (k /. relativeKey) config
    >>= \case
      Left a -> return $ Right customConfig
      Right a -> return $ Right $ f a customConfig
findKeyAndApplyConfig config k relativeKey f (Left e) = return $ Left e
