{-# LANGUAGE DeriveGeneric #-}
module Main where

import Conferer
import Conferer.FromConfig.Warp ()

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (Settings, runSettings, getPort, setPort)
import GHC.Generics

data AppConfig = AppConfig
  { appConfigWarp :: Settings
  , appConfigSeed :: Int
  } deriving (Generic)
instance FromConfig AppConfig
instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigWarp = setPort 2222 configDef -- If you want to configure new default for internal libs this is the place
    , appConfigSeed = 17
    }

main :: IO ()
main = do
  config <- defaultConfig "awesomeapp"
  appConfig <- getFromRootConfig config

  putStrLn $ "Running on port: " ++ show (getPort $ appConfigWarp appConfig)
  runSettings (appConfigWarp appConfig) application

application :: Application
application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

