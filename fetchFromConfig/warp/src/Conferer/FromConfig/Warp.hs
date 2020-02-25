{-# LANGUAGE FlexibleInstances #-}
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

import Conferer.Core
import Conferer.Types
import Conferer.FromConfig.Basics
import Data.Maybe (catMaybes)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
import Data.String (fromString)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)

instance DefaultConfig HostPreference
instance FromConfig HostPreference where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigByIsString

instance DefaultConfig ProxyProtocol
instance FromConfig ProxyProtocol where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigWith
    (\text -> case text of
                "ProxyProtocolNone" -> Just ProxyProtocolNone
                "ProxyProtocolRequired" -> Just ProxyProtocolRequired
                "ProxyProtocolOptional" -> Just ProxyProtocolOptional
                _ -> Nothing
    )

instance DefaultConfig Settings where
  configDef = defaultSettings

instance FromConfig Settings where
  fetchFromConfig key config = return Nothing
  updateFromConfig key config settings = do
    pure settings
      >>= findKeyAndApplyConfig config key "port" settingsPort (\v c -> c { settingsPort = v })
      >>= findKeyAndApplyConfig config key "host" settingsHost (\v c -> c { settingsHost = v })
      >>= findKeyAndApplyConfig config key "timeout" settingsTimeout (\v c -> c { settingsTimeout = v })
      >>= findKeyAndApplyConfig config key "fdCacheDuration" settingsFdCacheDuration (\v c -> c { settingsFdCacheDuration = v })
      >>= findKeyAndApplyConfig config key "fileInfoCacheDuration" settingsFileInfoCacheDuration (\v c -> c { settingsFileInfoCacheDuration = v })
      >>= findKeyAndApplyConfig config key "noParsePath" settingsNoParsePath (\v c -> c { settingsNoParsePath = v })
      >>= findKeyAndApplyConfig config key "serverName" settingsServerName (\v c -> c { settingsServerName = v })
      >>= findKeyAndApplyConfig config key "maximumBodyFlush" settingsMaximumBodyFlush (\v c -> c { settingsMaximumBodyFlush = v })
      >>= findKeyAndApplyConfig config key "proxyProtocol" settingsProxyProtocol (\v c -> c { settingsProxyProtocol = v })
      >>= findKeyAndApplyConfig config key "slowlorisSize" settingsSlowlorisSize (\v c -> c { settingsSlowlorisSize = v })
      >>= findKeyAndApplyConfig config key "http2Enabled" settingsHTTP2Enabled (\v c -> c { settingsHTTP2Enabled = v })
      >>= findKeyAndApplyConfig config key "gracefulShutdownTimeout" settingsGracefulShutdownTimeout (\v c -> c { settingsGracefulShutdownTimeout = v })
      >>= findKeyAndApplyConfig config key "gracefulCloseTimeout1" settingsGracefulCloseTimeout1 (\v c -> c { settingsGracefulCloseTimeout1 = v })
      >>= findKeyAndApplyConfig config key "gracefulCloseTimeout2" settingsGracefulCloseTimeout2 (\v c -> c { settingsGracefulCloseTimeout2 = v })
      -- >>= findKeyAndApplyConfig config key "maxTotalHeaderLength" settingsMaxTotalHeaderLength (\v c -> c { settingsMaxTotalHeaderLength = v })
