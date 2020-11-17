{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Conferer.FromConfig.Warp
  (
  -- * How to use this
  -- | FromConfig instance for warp server settings
  --
  -- @
  -- import Conferer
  -- import Conferer.FromConfig.Warp ()
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   warpSettings <- 'getFromConfig' \"warp\" config
  -- @
  ) where

import Conferer.Config
import Conferer.FromConfig.Internal
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal

instance FromConfig HostPreference where
  fetchFromConfig = fetchFromConfigByIsString

instance FromConfig ProxyProtocol where
  fetchFromConfig = fetchFromConfigWith
    (\case
      "ProxyProtocolNone" -> Just ProxyProtocolNone
      "ProxyProtocolRequired" -> Just ProxyProtocolRequired
      "ProxyProtocolOptional" -> Just ProxyProtocolOptional
      _ -> Nothing
    )

-- instance DefaultConfig Settings where
--   configDef = defaultSettings

instance FromConfig Settings where
  fetchFromConfig key config = do
    Settings{..} <- fetchFromDefaults key config
    settingsPort <- getFromConfigWithDefault (key /. "port") config settingsPort
    settingsHost <- getFromConfigWithDefault (key /. "host") config settingsHost
    settingsOnException <- getFromConfigWithDefault (key /. "onException") config settingsOnException
    settingsOnExceptionResponse <- getFromConfigWithDefault (key /. "onExceptionResponse") config settingsOnExceptionResponse
    settingsOnOpen <- getFromConfigWithDefault (key /. "onOpen") config settingsOnOpen
    settingsOnClose <- getFromConfigWithDefault (key /. "onClose") config settingsOnClose
    settingsTimeout <- getFromConfigWithDefault (key /. "timeout") config settingsTimeout
    settingsManager <- getFromConfigWithDefault (key /. "manager") config settingsManager
    settingsFdCacheDuration <- getFromConfigWithDefault (key /. "fdCacheDuration") config settingsFdCacheDuration
    settingsFileInfoCacheDuration <- getFromConfigWithDefault (key /. "fileInfoCacheDuration") config settingsFileInfoCacheDuration
    settingsBeforeMainLoop <- getFromConfigWithDefault (key /. "beforeMainLoop") config settingsBeforeMainLoop
#if MIN_VERSION_warp(3,0,4)
    -- settingsFork <- getFromConfigWithDefault (key /. "fork") config settingsFork
#endif
#if MIN_VERSION_warp(2,0,3)
    settingsNoParsePath <- getFromConfigWithDefault (key /. "noParsePath") config settingsNoParsePath
#endif
#if MIN_VERSION_warp(3,0,1)
    settingsInstallShutdownHandler <- getFromConfigWithDefault (key /. "installShutdownHandler") config settingsInstallShutdownHandler
#endif
#if MIN_VERSION_warp(3,0,2)
    settingsServerName <- getFromConfigWithDefault (key /. "serverName") config settingsServerName
#endif
#if MIN_VERSION_warp(3,0,3)
    settingsMaximumBodyFlush <- getFromConfigWithDefault (key /. "maximumBodyFlush") config settingsMaximumBodyFlush
#endif
#if MIN_VERSION_warp(3,0,5)
    settingsProxyProtocol <- getFromConfigWithDefault (key /. "proxyProtocol") config settingsProxyProtocol
#endif
#if MIN_VERSION_warp(3,1,2)
    settingsSlowlorisSize <- getFromConfigWithDefault (key /. "slowlorisSize") config settingsSlowlorisSize
#endif
#if MIN_VERSION_warp(3,1,7)
    settingsHTTP2Enabled <- getFromConfigWithDefault (key /. "http2Enabled") config settingsHTTP2Enabled
#endif
#if MIN_VERSION_warp(3,1,10)
    settingsLogger <- getFromConfigWithDefault (key /. "logger") config settingsLogger
#endif
#if MIN_VERSION_warp(3,2,7)
    settingsServerPushLogger <- getFromConfigWithDefault (key /. "serverPushLogger") config settingsServerPushLogger
#endif
#if MIN_VERSION_warp(3,2,8)
    settingsGracefulShutdownTimeout <- getFromConfigWithDefault (key /. "gracefulShutdownTimeout") config settingsGracefulShutdownTimeout
#endif
#if MIN_VERSION_warp(3,3,5)
    settingsGracefulCloseTimeout1 <- getFromConfigWithDefault (key /. "gracefulCloseTimeout1") config settingsGracefulCloseTimeout1
    settingsGracefulCloseTimeout2 <- getFromConfigWithDefault (key /. "gracefulCloseTimeout2") config settingsGracefulCloseTimeout2
#endif
#if MIN_VERSION_warp(3,3,8)
    settingsMaxTotalHeaderLength <- getFromConfigWithDefault (key /. "maxTotalHeaderLength") config settingsMaxTotalHeaderLength
#endif
#if MIN_VERSION_warp(3,3,11)
    settingsAltSvc <- getFromConfigWithDefault (key /. "altSvc") config settingsAltSvc
#endif
    return Settings{..}
