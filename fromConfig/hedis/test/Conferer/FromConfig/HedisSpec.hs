{-# LANGUAGE CPP #-}
module Conferer.FromConfig.HedisSpec where

import           Test.Hspec
import           Conferer.Types
import           Data.Text
import           Conferer
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
  let defaultPort = Redis.connectPort configDef
      defaultHost = Redis.connectMaxConnections configDef
  describe "fetching a hedis configuration without overriding anything" $ do
    it "returns hedis default config" $ do
      let config = emptyConfig
      fetchedValue <- getFromConfig "." config
      fetchedValue `portAndHostShouldBe` (defaultPort, defaultHost)
  describe "fetching a hedis configuration overriding its port" $ do
    it "returns a hedis config with its port set to the overriden one" $ do
      config <- configWith [("hedis.port", "9999")]
      fetchedValue <- getFromConfig "hedis" config
      fetchedValue `portAndHostShouldBe` (Redis.PortNumber 9999, defaultHost)
  describe "fetching a hedis configuration overriding its host" $ do
    it "returns a hedis config with its host set to the overriden one" $ do
      config <- configWith [("hedis.maxConnections", "6")]
      fetchedValue <- getFromConfig "hedis" config
      fetchedValue `portAndHostShouldBe` (defaultPort, 6)

#if MIN_VERSION_hedis(0,10,0)
  describe "fetching a hedis configuration overriding its host" $ do
    it "returns a hedis config with its host set to the overriden one" $ do
      config <- configWith [("hedis", "redis://username:password@host:42")]
      fetchedValue <- getFromConfig "hedis" config
      fetchedValue `portAndHostShouldBe` (Redis.PortNumber 42, defaultHost)
  describe "fetching a hedis configuration overriding its host" $ do
    it "returns a hedis config with its host set to the overriden one" $ do
      config <- configWith
        [ ("hedis", "redis://username:password@host:42")
        , ("hedis.maxConnections", "70")
        ]
      fetchedValue <- getFromConfig "hedis" config
      fetchedValue `portAndHostShouldBe` (Redis.PortNumber 42, 70)
#endif
