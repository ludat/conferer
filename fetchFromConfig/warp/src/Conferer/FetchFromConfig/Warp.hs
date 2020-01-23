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

instance FetchFromConfig HostPreference where
  fetch = fetchFromConfigWith (pure . fromString . unpack)

instance FetchFromConfig Settings where
  fetch k config = do
    port <- fetch (k /. "port") config
    timeout <- fetch (k /. "timeout") config
    host <- fetch (k /. "host") config
    serverName <- fetch (k /. "serverName") config
    let overriders = catMaybes [
                      setTimeout <$> timeout,
                      setHost <$> host,
                      setPort <$> port,
                      setServerName <$> serverName
                    ]
    return $ return $ foldr ($) defaultSettings overriders
