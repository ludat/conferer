{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Conferer.FromConfig.Hedis
  (
  -- * How to use this
  -- | FromConfig instance for hedis server settings
  --
  -- @
  -- import Conferer
  -- import Conferer.FromConfig.Hedis ()
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   hedisSettings <- 'getFromConfig' \"hedis\" config
  -- @
  ) where

import Conferer.Core
import Conferer.Types
import Conferer.FromConfig.Basics
import qualified Database.Redis as Redis
import Data.Text (unpack)
import Text.Read (readMaybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep)
import Control.Exception (throwIO)

instance FromConfig Redis.PortID where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig = fetchFromConfigWith (\t -> do
      case readMaybe $ unpack t of
        Just n -> return $ Redis.PortNumber n
        Nothing -> do
#ifdef mingw32_HOST_OS
          Nothing
#else
          return $ Redis.UnixSocket $ unpack t
#endif
    )

instance DefaultConfig Redis.ConnectInfo where
  configDef = Redis.defaultConnectInfo

instance FromConfig Redis.ConnectInfo where
  fetchFromConfig _key _config = do
    return Nothing

  updateFromConfig key config connectInfo = do
    redisConfig <-
-- For hedis < 0.10.0 `Redis.parseConnectInfo` doesn't exist so in that case
-- we simply avoid reading the url directly from key, and instead we directly
-- act as if it wasn't present
#if MIN_VERSION_hedis(0,10,0)
      getKey key config >>= \case
        Just connectionString ->
          case Redis.parseConnectInfo $ unpack connectionString of
            Right con -> return $ con
            Left _e ->
                throwIO $ ConfigParsingError key connectionString (typeRep (Proxy :: Proxy (Redis.ConnectInfo)))
        Nothing ->
#endif
          pure connectInfo
            >>= findKeyAndApplyConfig config key "host" Redis.connectHost (\v c -> c { Redis.connectHost = v })
            >>= findKeyAndApplyConfig config key "port" Redis.connectPort (\v c -> c { Redis.connectPort = v })
            >>= findKeyAndApplyConfig config key "auth" Redis.connectAuth (\v c -> c { Redis.connectAuth = v })

    pure redisConfig
      >>= findKeyAndApplyConfig config key "maxConnections" Redis.connectMaxConnections (\v c -> c { Redis.connectMaxConnections = v })
      -- >>= findKeyAndApplyConfig config key "maxIdleTime" Redis.connectMaxIdleTime (\v c -> c { Redis.connectMaxIdleTime = v })
      >>= return
