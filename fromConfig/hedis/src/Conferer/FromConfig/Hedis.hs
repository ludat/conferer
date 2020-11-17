{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

import Conferer.FromConfig
import qualified Database.Redis as Redis
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

instance FromConfig Redis.PortID where
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

instance FromConfig Redis.ConnectInfo where
  fetchFromConfig key config = do
    Redis.ConnInfo{..} <- fetchFromDefaults key config

-- For hedis < 0.10.0 `Redis.parseConnectInfo` doesn't exist so in that case
-- we simply avoid reading the url directly from key, and instead we directly
-- act as if it wasn't present
#if MIN_VERSION_hedis(0,10,0)
    (connectHost, connectPort, connectAuth, connectDatabase) <-
      fetchFromConfig @(Maybe Text) (key /. "url") config
        >>= \case
        Just connectionString -> do
          case Redis.parseConnectInfo $ unpack connectionString of
            Right Redis.ConnInfo{..} -> do
              return (connectHost, connectPort, connectAuth, connectDatabase)
            Left _e ->
              throwConfigParsingError @Redis.ConnectInfo key connectionString
        Nothing -> do
          return (connectHost, connectPort, connectAuth, connectDatabase)
#endif
    connectHost <- getFromConfigWithDefault (key /. "host") config connectHost
    connectPort <- getFromConfigWithDefault (key /. "port") config connectPort
    connectAuth <- getFromConfigWithDefault (key /. "auth") config connectAuth
    connectDatabase <- getFromConfigWithDefault (key /. "database") config connectDatabase

    connectMaxConnections <- getFromConfigWithDefault (key /. "maxConnections") config connectMaxConnections
    connectMaxIdleTime <- getFromConfigWithDefault (key /. "maxIdleTime") config connectMaxIdleTime
    connectTimeout <- getFromConfigWithDefault (key /. "timeout") config connectTimeout
    connectTLSParams <- getFromConfigWithDefault (key /. "tlsParams") config connectTLSParams

    pure Redis.ConnInfo{..}
