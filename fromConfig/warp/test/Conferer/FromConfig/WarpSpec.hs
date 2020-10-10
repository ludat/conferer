module Conferer.FromConfig.WarpSpec where

import           Test.Hspec
import           Conferer.Types
import           Data.Text
import           Conferer
import           Conferer.FromConfig.Warp ()
import           Network.Wai.Handler.Warp

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addSource (mkMapSource keyValues)

fetch :: (FromConfig a, DefaultConfig a) => Key -> Config -> IO (Maybe a)
fetch = safeGetFromConfig

portAndHostShouldBe :: Maybe Settings -> (Port, HostPreference) -> Expectation
portAndHostShouldBe fetchedSettings (port, host) = do
  getPort <$> fetchedSettings `shouldBe` Just port
  getHost <$> fetchedSettings `shouldBe` Just host

spec :: Spec
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
