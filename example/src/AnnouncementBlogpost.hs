{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module AnnouncementBlogpost where

import qualified Conferer
import Conferer.FromConfig.Warp ()
import GHC.Generics

import Network.Wai
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as Warp

import qualified Conferer.Source.CLIArgs as Cli
import qualified Conferer.Source.Env as Env
import qualified Conferer.Source.Dhall as Dhall

data AppConfig = AppConfig
  { appConfigServer :: Warp.Settings
  , appConfigBanner :: String
  } deriving (Generic)
instance Conferer.FromConfig AppConfig
instance Conferer.DefaultConfig AppConfig where
 configDef = AppConfig
   { appConfigServer = Conferer.configDef
   , appConfigBanner = "Hi conferer"
   }

mkMyConfig :: IO Conferer.Config
mkMyConfig = Conferer.mkConfig' []
  [ Cli.fromConfig
  , Env.fromConfig "myapp"
  , Dhall.fromFilePath "config.dhall"
  ]


announcementMain :: IO ()
announcementMain = do
  config <- mkMyConfig
  appConfig <- Conferer.fetch config
  let warpSettings = appConfigServer appConfig

  putStrLn $ appConfigBanner appConfig
  putStrLn $ "Running on port: " ++ show (Warp.getPort $ warpSettings)
  Warp.runSettings warpSettings application

application :: Application
application _ respond = do
 respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hi"
