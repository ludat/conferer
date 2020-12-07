{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.SnapSpec where

import Test.Hspec

import Conferer
import Conferer.Config
import Conferer.Test
import Conferer.FromConfig.Snap ()
import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Internal.Core as Snap
import Snap.Http.Server.Config hiding (Config, emptyConfig)
import Snap.Internal.Core
import Data.Text (Text)

spec :: Spec
spec = do
  describe "fetching a snap configuration that has the default" $ do
    it "returns snap default config" $ do
      config <- configWith [] []
      fetchedValue <- fetchKey @(Snap.Config Snap ()) "snap" config configDef
      getPort fetchedValue
        `shouldBe` getPort (configDef :: Snap.Config Snap ())
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [] [("snap.port", "5555")]
      fetchedValue <- fetchKey @(Snap.Config Snap ()) "snap" config configDef
      getPort fetchedValue
        `shouldBe` Just 5555
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [] []
      fetchedValue <- fetchKey @(Snap.Config Snap ()) "snap" config
        (setPort 5555 (configDef @(Snap.Config Snap ())))
      getPort fetchedValue
        `shouldBe` Just 5555
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [] [("snap.other", "55")]
      fetchedValue <- fetchKey @(Snap.Config Snap Int) "snap" config configDef
      getOther fetchedValue
        `shouldBe` Just 55

