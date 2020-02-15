{-# LANGUAGE FlexibleInstances #-}
module Conferer.FetchFromConfig.Warp
  (
  -- * How to use this
  -- | FetchFromConfig instance for warp server settings
  --
  -- @
  -- import Conferer
  -- import Conferer.FetchFromConfig.Warp ()
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   warpSettings <- 'getFromConfig' \"warp\" config
  -- @
  ) where

import Conferer.Core
import Conferer.Types
import Conferer.FetchFromConfig.Basics
import Data.Maybe (catMaybes)
import Network.Wai.Handler.Warp
import Data.String (fromString)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)

instance DefaultConfig HostPreference
instance UpdateFromConfig HostPreference where
  updateFromConfig = updateFromConfigWith (pure . fromString . unpack)

instance DefaultConfig Settings where
  configDef = defaultSettings


instance UpdateFromConfig Settings where
  updateFromConfig k config settings = do
    pure settings
      >>= findKeyAndApplyConfig config k "port" getPort setPort
      -- >>= findKeyAndApplyConfig config k "timeout" Snap.getAccessLog (withMaybe Snap.setAccessLog)
      -- >>= findKeyAndApplyConfig config k "host" Snap.getBind (withMaybe Snap.setBind)
      -- >>= findKeyAndApplyConfig config k "serverName" Snap.getCompression (setCompression)
  -- fetch k config = do
  --   port <- fetch (k /. "port") config
  --   timeout <- fetch (k /. "timeout") config
  --   host <- fetch (k /. "host") config
  --   serverName <- fetch (k /. "serverName") config
  --   let overriders = catMaybes [
  --                     setTimeout <$> timeout,
  --                     setHost <$> host,
  --                     setPort <$> port,
  --                     setServerName <$> serverName
  --                   ]
  --   return $ return $ foldr ($) defaultSettings overriders
