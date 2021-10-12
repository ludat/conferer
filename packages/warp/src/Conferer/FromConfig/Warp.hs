-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for warp
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Conferer.FromConfig.Warp where

import qualified Data.Text as Text
import Data.Dynamic

import Conferer.FromConfig

import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal

instance FromConfig HostPreference where
  fromConfig = fetchFromConfigByIsString

instance FromConfig ProxyProtocol where
  fromConfig = fetchFromConfigWith $
    (\case
      "proxyprotocolnone" -> Just ProxyProtocolNone
      "none" -> Just ProxyProtocolNone
      "proxyprotocolrequired" -> Just ProxyProtocolRequired
      "required" -> Just ProxyProtocolRequired
      "proxyprotocoloptional" -> Just ProxyProtocolOptional
      "optional" -> Just ProxyProtocolOptional
      _ -> Nothing
    ) . Text.toLower

instance DefaultConfig Settings where
  configDef = defaultSettings

-- | Deconstruct a 'Settings' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructSettingsToDefaults :: Settings -> [(Key, Dynamic)]
deconstructSettingsToDefaults Settings{..} =
  [ ("port", toDyn settingsPort)
  , ("host", toDyn settingsHost)
  , ("onException", toDyn settingsOnException)
  , ("onExceptionResponse", toDyn settingsOnExceptionResponse)
  , ("onOpen", toDyn settingsOnOpen)
  , ("onClose", toDyn settingsOnClose)
  , ("timeout", toDyn settingsTimeout)
  , ("manager", toDyn settingsManager)
  , ("fdCacheDuration", toDyn settingsFdCacheDuration)
  , ("fileInfoCacheDuration", toDyn settingsFileInfoCacheDuration)
  , ("beforeMainLoop", toDyn settingsBeforeMainLoop)
#if MIN_VERSION_warp(3,0,4)
  , ("fork", toDyn $ ForkSettings settingsFork)
#endif
#if MIN_VERSION_warp(2,0,3)
  , ("noParsePath", toDyn settingsNoParsePath)
#endif
#if MIN_VERSION_warp(3,0,1)
  , ("installShutdownHandler", toDyn settingsInstallShutdownHandler)
#endif
#if MIN_VERSION_warp(3,0,2)
  , ("serverName", toDyn settingsServerName)
#endif
#if MIN_VERSION_warp(3,0,3)
  , ("maximumBodyFlush", toDyn settingsMaximumBodyFlush)
#endif
#if MIN_VERSION_warp(3,0,5)
  , ("proxyProtocol", toDyn settingsProxyProtocol)
#endif
#if MIN_VERSION_warp(3,1,2)
  , ("slowlorisSize", toDyn settingsSlowlorisSize)
#endif
#if MIN_VERSION_warp(3,1,7)
  , ("http2Enabled", toDyn settingsHTTP2Enabled)
#endif
#if MIN_VERSION_warp(3,1,10)
  , ("logger", toDyn settingsLogger)
#endif
#if MIN_VERSION_warp(3,2,7)
  , ("serverPushLogger", toDyn settingsServerPushLogger)
#endif
#if MIN_VERSION_warp(3,2,8)
  , ("gracefulShutdownTimeout", toDyn settingsGracefulShutdownTimeout)
#endif
#if MIN_VERSION_warp(3,3,5)
  , ("gracefulCloseTimeout1", toDyn settingsGracefulCloseTimeout1)
  , ("gracefulCloseTimeout2", toDyn settingsGracefulCloseTimeout2)
#endif
#if MIN_VERSION_warp(3,3,8)
  , ("maxTotalHeaderLength", toDyn settingsMaxTotalHeaderLength)
#endif
#if MIN_VERSION_warp(3,3,11)
  , ("altSvc", toDyn settingsAltSvc)
#endif
  ]

-- | Newtype wrapper for the 'settingsFork' value that has too much polymorphism
-- to be typeable
newtype ForkSettings = ForkSettings (((forall a. IO a -> IO a) -> IO ()) -> IO ())

instance FromConfig Settings where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructSettingsToDefaults key originalConfig
    settingsPort <- fetchFromConfig (key /. "port") config
    settingsHost <- fetchFromConfig (key /. "host") config
    NotUserConfigurable settingsOnException <- fetchFromConfig (key /. "onException") config
    NotUserConfigurable settingsOnExceptionResponse <- fetchFromConfig (key /. "onExceptionResponse") config
    NotUserConfigurable settingsOnOpen <- fetchFromConfig (key /. "onOpen") config
    NotUserConfigurable settingsOnClose <- fetchFromConfig (key /. "onClose") config
    settingsTimeout <- fetchFromConfig (key /. "timeout") config
    NotUserConfigurable settingsManager <- fetchFromConfig (key /. "manager") config
    settingsFdCacheDuration <- fetchFromConfig (key /. "fdCacheDuration") config
    settingsFileInfoCacheDuration <- fetchFromConfig (key /. "fileInfoCacheDuration") config
    NotUserConfigurable settingsBeforeMainLoop <- fetchFromConfig (key /. "beforeMainLoop") config
#if MIN_VERSION_warp(3,0,4)
    NotUserConfigurable (ForkSettings settingsFork) <- fetchFromConfig (key /. "fork") config
#endif
#if MIN_VERSION_warp(2,0,3)
    settingsNoParsePath <- fetchFromConfig (key /. "noParsePath") config
#endif
#if MIN_VERSION_warp(3,0,1)
    NotUserConfigurable settingsInstallShutdownHandler <- fetchFromConfig (key /. "installShutdownHandler") config
#endif
#if MIN_VERSION_warp(3,0,2)
    settingsServerName <- fetchFromConfig (key /. "serverName") config
#endif
#if MIN_VERSION_warp(3,0,3)
    settingsMaximumBodyFlush <- fetchFromConfig (key /. "maximumBodyFlush") config
#endif
#if MIN_VERSION_warp(3,0,5)
    settingsProxyProtocol <- fetchFromConfig (key /. "proxyProtocol") config
#endif
#if MIN_VERSION_warp(3,1,2)
    settingsSlowlorisSize <- fetchFromConfig (key /. "slowlorisSize") config
#endif
#if MIN_VERSION_warp(3,1,7)
    settingsHTTP2Enabled <- fetchFromConfig (key /. "http2Enabled") config
#endif
#if MIN_VERSION_warp(3,1,10)
    NotUserConfigurable settingsLogger <- fetchFromConfig (key /. "logger") config
#endif
#if MIN_VERSION_warp(3,2,7)
    NotUserConfigurable settingsServerPushLogger <- fetchFromConfig (key /. "serverPushLogger") config
#endif
#if MIN_VERSION_warp(3,2,8)
    settingsGracefulShutdownTimeout <- fetchFromConfig (key /. "gracefulShutdownTimeout") config
#endif
#if MIN_VERSION_warp(3,3,5)
    settingsGracefulCloseTimeout1 <- fetchFromConfig (key /. "gracefulCloseTimeout1") config
    settingsGracefulCloseTimeout2 <- fetchFromConfig (key /. "gracefulCloseTimeout2") config
#endif
#if MIN_VERSION_warp(3,3,8)
    settingsMaxTotalHeaderLength <- fetchFromConfig (key /. "maxTotalHeaderLength") config
#endif
#if MIN_VERSION_warp(3,3,11)
    settingsAltSvc <- fetchFromConfig (key /. "altSvc") config
#endif
    return Settings{..}
