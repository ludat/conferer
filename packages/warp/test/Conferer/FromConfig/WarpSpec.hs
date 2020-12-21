{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.WarpSpec where

import Test.Hspec
import Network.Wai.Handler.Warp

import Conferer
import Conferer.Test
import Conferer.FromConfig.Warp ()

portAndHostShouldBe :: Settings -> (Port, HostPreference) -> Expectation
portAndHostShouldBe fetchedSettings (port, host) = do
  getPort fetchedSettings `shouldBe` port
  getHost fetchedSettings `shouldBe` host

spec :: Spec
spec = do
  let defaultPort = getPort configDef
      defaultHost = getHost configDef

  describe "fetching a warp configuration from a totally empty config" $ do
    it "throws an exception" $ do
      config <- configWith [] []
      unsafeFetchKey @Settings "warp" config
        `shouldThrow` anyException
  describe "fetching a warp configuration that has the default" $ do
    it "returns warp default config" $ do
      config <- configWith [] []
      fetchedValue <- fetchKey "warp" config configDef
      fetchedValue `portAndHostShouldBe` (defaultPort, defaultHost)
  describe "fetching a warp configuration overridnig its port" $ do
    it "returns a warp config with its port set to the overriden one" $ do
      config <- configWith [] [("warp.port", "9999")]
      fetchedValue <- fetchKey "warp" config configDef
      fetchedValue `portAndHostShouldBe` (9999, defaultHost)
  describe "fetching a warp configuration overriding its host" $ do
    it "returns a warp config with its host set to the overriden one" $ do
      config <- configWith [] [("warp.host", "!6")]
      fetchedValue <- fetchKey "warp" config configDef
      fetchedValue `portAndHostShouldBe` (defaultPort, "!6")
