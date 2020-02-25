module Main where

import Conferer
import Conferer.FromConfig.Warp ()

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (runSettings, getPort)

main :: IO ()
main = do
  config <- defaultConfig "awesomeapp"
  warpSettings <- getFromConfig "warp" config

  putStrLn $ "Running on port: " ++ show (getPort warpSettings)
  runSettings warpSettings application

application :: Application
application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

