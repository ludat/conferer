module Conferer.FetchFromConfig.WarpSpec where

import           Test.Hspec
import           Conferer.Types
import           Data.Text
import           Conferer
import           Conferer.FetchFromConfig.Warp
import Network.Wai.Handler.Warp

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addProvider (mkMapConfigProvider keyValues)

portAndHostShouldBe :: Either Text Settings -> (Port, HostPreference) -> Expectation
portAndHostShouldBe fetchedSettings (port, host) = do
  getPort <$> fetchedSettings `shouldBe` Right port
  getHost <$> fetchedSettings `shouldBe` Right host

spec = do
  let defaultPort = getPort defaultSettings
      defaultHost = getHost defaultSettings
  describe "fetching a warp configuration without overriding anything" $ do
    it "returns warp default config" $ do
      let config = emptyConfig
      fetchedValue <- fetch "." config
      fetchedValue `portAndHostShouldBe` (defaultPort, defaultHost)
  describe "fetching a warp configuration overridnig its port" $ do
    it "returns a warp config with its port set to the overriden one" $ do
      config <- configWith [("warp.port", "9999")]
      fetchedValue <- fetch "warp" config
      fetchedValue `portAndHostShouldBe` (9999, defaultHost)
  describe "fetching a warp configuration overriding its host" $ do
    it "returns a warp config with its host set to the overriden one" $ do
      config <- configWith [("warp.host", "!6")]
      fetchedValue <- fetch "warp" config
      fetchedValue `portAndHostShouldBe` (defaultPort, "!6")
