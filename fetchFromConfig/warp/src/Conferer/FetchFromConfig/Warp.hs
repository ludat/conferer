{-# LANGUAGE FlexibleInstances #-}
module Conferer.FetchFromConfig.Warp where

import Conferer.Core
import Conferer.Types
import Conferer.FetchFromConfig.Basics
import Data.Either (rights)
import Network.Wai.Handler.Warp
import Data.String (fromString)
import Data.Text (unpack)

instance FetchFromConfig HostPreference where
    fetch = fetchFromConfigWith (pure . fromString . unpack)

instance FetchFromConfig Settings where
    fetch k config = do
        port <- fetch (k /. "port") config
        timeout <- fetch (k /. "timeout") config
        host <- fetch (k /. "host") config
        serverName <- fetch (k /. "serverName") config
        let overriders = rights [
                          setTimeout <$> timeout,
                          setHost <$> host,
                          setPort <$> port,
                          setServerName <$> serverName
                        ]
        return . return $ foldr ($) defaultSettings overriders
