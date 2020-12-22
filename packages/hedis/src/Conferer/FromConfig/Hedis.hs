-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for hedis
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Conferer.FromConfig.Hedis where

import Conferer.FromConfig
import Conferer.Config

import qualified Database.Redis as Redis
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import Data.Dynamic

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

-- | Deconstruct a 'Redis.ConnectInfo' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructConnInfoToDefaults :: Redis.ConnectInfo -> [(Key, Dynamic)]
deconstructConnInfoToDefaults Redis.ConnInfo{..} =
  [ ("host", toDyn connectHost)
  , ("port", toDyn connectPort)
  , ("auth", toDyn connectAuth)
  , ("database", toDyn connectDatabase)

  , ("maxConnections", toDyn connectMaxConnections)
  , ("maxIdleTime", toDyn connectMaxIdleTime)
  , ("timeout", toDyn connectTimeout)
#if MIN_VERSION_hedis(0,10,2)
  , ("tlsParams", toDyn connectTLSParams)
#endif
  ]

instance DefaultConfig Redis.ConnectInfo where
  configDef = Redis.defaultConnectInfo

instance FromConfig Redis.ConnectInfo where
  fetchFromConfig key originalConfig = do
    firstConfig <- addDefaultsAfterDeconstructingToDefaults deconstructConnInfoToDefaults key originalConfig

-- For hedis < 0.10.0 `Redis.parseConnectInfo` doesn't exist so in that case
-- we simply avoid reading the url directly from key, and instead we directly
-- act as if it wasn't present
#if MIN_VERSION_hedis(0,10,0)
    config <-
      fetchFromConfig @(Maybe Text) (key /. "url") firstConfig
        >>= \case
        Just connectionString -> do
          case Redis.parseConnectInfo $ unpack connectionString of
            Right Redis.ConnInfo{..} -> do
              return $
                firstConfig
                & addDefaults
                    [ (key /. "host", toDyn connectHost)
                    , (key /. "port", toDyn connectPort)
                    , (key /. "auth", toDyn connectAuth)
                    , (key /. "database", toDyn connectDatabase)
                    ]
            Left _e ->
              throwConfigParsingError @Redis.ConnectInfo key connectionString
        Nothing -> do
          return firstConfig
#else
    config <- return firstConfig
#endif
    connectHost <- fetchFromConfig (key /. "host") config
    connectPort <- fetchFromConfig (key /. "port") config
    connectAuth <- fetchFromConfig (key /. "auth") config
    connectDatabase <- fetchFromConfig (key /. "database") config

    connectMaxConnections <- fetchFromConfig (key /. "maxConnections") config
    connectMaxIdleTime <- fetchFromConfig (key /. "maxIdleTime") config
    connectTimeout <- fetchFromConfig (key /. "timeout") config
#if MIN_VERSION_hedis(0,10,2)
    connectTLSParams <- fetchFromConfig (key /. "tlsParams") config
#endif
    pure Redis.ConnInfo{..}
