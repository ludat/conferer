{-# LANGUAGE CPP #-}
module Conferer.FromConfig.HedisSpec where

import           Test.Hspec
import           Conferer
import           Data.Text
import           Conferer.FromConfig.Hedis ()
import qualified Database.Redis as Redis

configWith :: [(Key, Text)] -> IO Config
configWith keyValues = emptyConfig & addSource (mkMapSource keyValues)

portAndHostShouldBe :: Redis.ConnectInfo -> (Redis.PortID, Int) -> Expectation
portAndHostShouldBe fetchedSettings (port, host) = do
  Redis.connectPort fetchedSettings `shouldBe` port
  Redis.connectMaxConnections fetchedSettings `shouldBe` host

spec :: Spec
spec = do
  let defaultPort = Redis.connectPort Redis.defaultConnectInfo
      defaultHost = Redis.connectMaxConnections Redis.defaultConnectInfo
  describe "fetching a hedis configuration without overriding anything" $ do
    it "returns hedis default config" $ do
      let config = emptyConfig
      fetchedValue <- getFromConfigWithDefault "" config Redis.defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (defaultPort, defaultHost)
  describe "fetching a hedis configuration overriding its port" $ do
    it "returns a hedis config with its port set to the overriden one" $ do
      config <- configWith [("hedis.port", "9999")]
      fetchedValue <- getFromConfigWithDefault "hedis" config Redis.defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (Redis.PortNumber 9999, defaultHost)
  describe "fetching a hedis configuration overriding its host" $ do
    it "returns a hedis config with its host set to the overriden one" $ do
      config <- configWith [("hedis.maxConnections", "6")]
      fetchedValue <- getFromConfigWithDefault "hedis" config Redis.defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (defaultPort, 6)

#if MIN_VERSION_hedis(0,10,0)
  describe "fetching a hedis configuration overriding its host" $ do
    it "returns a hedis config with its host set to the overriden one" $ do
      config <- configWith [("hedis.url", "redis://username:password@host:42")]
      fetchedValue <- getFromConfigWithDefault "hedis" config Redis.defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (Redis.PortNumber 42, defaultHost)
  describe "fetching a hedis configuration overriding its host" $ do
    it "returns a hedis config with its host set to the overriden one" $ do
      config <- configWith
        [ ("hedis.url", "redis://username:password@host:42")
        , ("hedis.maxConnections", "70")
        ]
      fetchedValue <- getFromConfigWithDefault "hedis" config Redis.defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (Redis.PortNumber 42, 70)
#endif
