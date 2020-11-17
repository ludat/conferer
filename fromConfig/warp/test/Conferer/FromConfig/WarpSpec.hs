{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.WarpSpec where

import           Test.Hspec
import           Data.Text
import           Conferer
import           Conferer.FromConfig.Warp ()
import           Network.Wai.Handler.Warp

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addDefault "warp" defaultSettings & addSource (mkMapSource keyValues) 

portAndHostShouldBe :: Settings -> (Port, HostPreference) -> Expectation
portAndHostShouldBe fetchedSettings (port, host) = do
  getPort fetchedSettings `shouldBe` port
  getHost fetchedSettings `shouldBe` host

spec :: Spec
spec = do
  let defaultPort = getPort defaultSettings
      defaultHost = getHost defaultSettings

  describe "fetching a warp configuration from a totally empty config" $ do
    it "throws an exception" $ do
      let config = emptyConfig
      getFromConfig @Settings "warp" config 
        `shouldThrow` anyException
  describe "fetching a warp configuration that has the default" $ do
    it "returns warp default config" $ do
      config <- configWith []
      fetchedValue <- getFromConfig "warp" config
      fetchedValue `portAndHostShouldBe` (defaultPort, defaultHost)
  describe "fetching a warp configuration overridnig its port" $ do
    it "returns a warp config with its port set to the overriden one" $ do
      config <- configWith [("warp.port", "9999")]
      fetchedValue <- getFromConfig "warp" config
      fetchedValue `portAndHostShouldBe` (9999, defaultHost)
  describe "fetching a warp configuration overriding its host" $ do
    it "returns a warp config with its host set to the overriden one" $ do
      config <- configWith [("warp.host", "!6")]
      fetchedValue <- getFromConfig "warp" config
      fetchedValue `portAndHostShouldBe` (defaultPort, "!6")
