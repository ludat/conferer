{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.SnapSpec where

import Test.Hspec

import Conferer hiding (Config)
import Conferer.Test
import Conferer.FromConfig.Snap ()
import Snap.Http.Server.Config
import Snap.Internal.Core

spec :: Spec
spec = do
  describe "fetching a snap configuration without a default" $ do
    it "returns snap empty config" $ do
      -- Snap config is based around lots of maybes so it possible to create
      -- one even if there is no default value
      config <- configWith [] []
      fetchedValue <- unsafeFetchKey @(Config Snap ()) "snap" config
      getPort fetchedValue
        `shouldBe` getPort (configDef :: Config Snap ())
  describe "fetching a snap configuration that has the default" $ do
    it "returns snap default config" $ do
      config <- configWith [] []
      fetchedValue <- fetchKey @(Config Snap ()) "snap" config configDef
      getPort fetchedValue
        `shouldBe` getPort (configDef :: Config Snap ())
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [] [("snap.port", "5555")]
      fetchedValue <- fetchKey @(Config Snap ()) "snap" config configDef
      getPort fetchedValue
        `shouldBe` Just 5555
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [] []
      fetchedValue <- fetchKey @(Config Snap ()) "snap" config
        (setPort 5555 (configDef @(Config Snap ())))
      getPort fetchedValue
        `shouldBe` Just 5555
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [] [("snap.other", "55")]
      fetchedValue <- fetchKey @(Config Snap Int) "snap" config configDef
      getOther fetchedValue
        `shouldBe` Just 55

