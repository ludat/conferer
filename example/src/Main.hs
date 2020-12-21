{-# LANGUAGE DeriveGeneric #-}
module Main where

import qualified Conferer
import Conferer.FromConfig.Warp ()

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (Settings, runSettings, getPort, setPort)
import GHC.Generics

data AppConfig = AppConfig
  { appConfigServer :: Settings
  , appConfigSeed :: Int
  } deriving (Generic)
instance Conferer.FromConfig AppConfig
instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigServer = setPort 2222 Conferer.configDef -- If you want to configure new default for internal libs this is the place
    , appConfigSeed = 17
    }

main :: IO ()
main = do
  config <- Conferer.mkConfig "awesomeapp"
  appConfig <- Conferer.fetch config Conferer.configDef

  putStrLn $ "Running on port: " ++ show (getPort $ appConfigServer appConfig)
  runSettings (appConfigServer appConfig) (application config)

application :: Conferer.Config -> Application
application config _ respond = do
  body <- Conferer.fetchKey "body" config "default"
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] body

