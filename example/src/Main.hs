{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Conferer
import Conferer.FromConfig.Warp ()

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, getPort, setPort)
import GHC.Generics

data AppConfig = AppConfig
  { appConfigServer :: Settings
  , appConfigSeed :: Int
  } deriving (Generic)
instance FromConfig AppConfig
-- instance DefaultConfig AppConfig where

configDef :: AppConfig
configDef = AppConfig
    { appConfigServer = setPort 2222 defaultSettings -- If you want to configure new default for internal libs this is the place
    , appConfigSeed = 17
    }

main :: IO ()
main = do
  config <- defaultConfig "awesomeapp"
  appConfig <- getFromRootConfigWithDefault config configDef

  putStrLn $ "Running on port: " ++ show (getPort $ appConfigServer appConfig)
  runSettings (appConfigServer appConfig) application

application :: Application
application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

