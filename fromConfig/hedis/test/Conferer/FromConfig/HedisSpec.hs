{-# LANGUAGE CPP #-}
module Conferer.FromConfig.HedisSpec where

import Test.Hspec
import Conferer.FromConfig.Hedis ()
import Database.Redis

import Conferer
import Conferer.Test

portAndHostShouldBe :: ConnectInfo -> (PortID, String) -> Expectation
portAndHostShouldBe fetchedSettings (port, host) = do
  connectPort fetchedSettings `shouldBe` port
  connectHost fetchedSettings `shouldBe` host

spec :: Spec
spec = do
  let defaultPort = connectPort defaultConnectInfo
      defaultHost = connectHost defaultConnectInfo
  describe "with no configuration" $ do
    it "returns hedis default config" $ do
      config <- configWith [] []
      fetchedValue <- fetchKey "hedis" config defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (defaultPort, defaultHost)

  describe "with configured port" $ do
    it "return a hedis config with that port" $ do
      config <- configWith [] [("hedis.port", "9999")]
      fetchedValue <- fetchKey "hedis" config defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (PortNumber 9999, defaultHost)

  describe "with configured maxConnections" $ do
    it "returns a hedis config with that max connections" $ do
      config <- configWith [] [("hedis.maxConnections", "6")]
      fetchedValue <- fetchKey "hedis" config defaultConnectInfo
      connectMaxConnections fetchedValue
        `shouldBe` 6


#if MIN_VERSION_hedis(0,10,0)
  describe "with a url configured" $ do
    it "returns a hedis config with host, port, auth and database from the url" $ do
      config <- configWith [] [("hedis.url", "redis://username:password@host:42")]
      fetchedValue <- fetchKey "hedis" config defaultConnectInfo
      fetchedValue `portAndHostShouldBe` (PortNumber 42, "host")

    describe "and something url defined configured by itself" $ do
      it "returns a hedis config with the more specific configured value" $ do
        config <- configWith
          []
          [ ("hedis.url", "redis://username:password@host:42")
          , ("hedis.port", "72")
          ]
        fetchedValue <- fetchKey "hedis" config defaultConnectInfo
        fetchedValue `portAndHostShouldBe` (PortNumber 72, "host")

    describe "and a something not defined in url" $ do
      it "returns a hedis config with that value as well" $ do
        config <- configWith
          []
          [ ("hedis.url", "redis://username:password@host:42")
          , ("hedis.maxConnections", "70")
          ]
        fetchedValue <- fetchKey "hedis" config defaultConnectInfo
        fetchedValue `portAndHostShouldBe` (PortNumber 42, "host")
        connectMaxConnections fetchedValue
          `shouldBe` 70

    describe "and a default not present in url" $ do
      it "returns a hedis config with the right default" $ do
        config <- configWith
          []
          [ ("hedis.url", "redis://username:password@host:42")
          ]
        fetchedValue <- fetchKey "hedis" config
          (defaultConnectInfo {connectMaxConnections = 73})
        fetchedValue `portAndHostShouldBe` (PortNumber 42, "host")
        connectMaxConnections fetchedValue
          `shouldBe` 73
#endif
