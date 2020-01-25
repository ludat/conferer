{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Conferer.FetchFromConfig.Hedis
  (
  -- * How to use this
  -- | FetchFromConfig instance for hedis server settings
  --
  -- @
  -- import Conferer
  -- import Conferer.FetchFromConfig.Hedis ()
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   hedisSettings <- 'getFromConfig' \"hedis\" config
  -- @
  ) where

import Conferer.Core
import Conferer.Types
import Conferer.FetchFromConfig.Basics
import Data.Maybe (catMaybes)
import qualified Database.Redis as Redis
import Data.String (fromString)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep)
import Control.Exception (throwIO)

instance FetchFromConfig Redis.PortID where
  fetch = fetchFromConfigWith (\t -> do
      case readMaybe $ unpack t of
        Just n -> return $ Redis.PortNumber n
        Nothing ->
          return $ Redis.UnixSocket $ unpack t
    )

instance DefaultConfig Redis.ConnectInfo where
  configDef = Redis.defaultConnectInfo

instance FetchFromConfig Redis.ConnectInfo where
  fetch key config = do
    redisConfig <- getKey key config
      >>= \case
        Just connectionString -> 
          case Redis.parseConnectInfo $ unpack connectionString of
            Right con -> return $ con
            Left e -> 
                throwIO $ ConfigParsingError key connectionString (typeRep (Proxy :: Proxy (Redis.ConnectInfo)))
        Nothing -> 
          pure configDef
            >>= findKeyAndApplyConfig config key "host" (\v c -> c { Redis.connectHost = v })
            >>= findKeyAndApplyConfig config key "port" (\v c -> c { Redis.connectPort = v })
            >>= findKeyAndApplyConfig config key "auth" (\v c -> c { Redis.connectAuth = v })

    pure redisConfig
      >>= findKeyAndApplyConfig config key "maxConnections" (\v c -> c { Redis.connectMaxConnections = v })
      >>= findKeyAndApplyConfig config key "maxIdleTime" (\v c -> c { Redis.connectMaxConnections = v })
      >>= (return . return)
