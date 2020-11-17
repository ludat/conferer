{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.SnapSpec where

import           Test.Hspec

import           Conferer hiding (defaultConfig)
import           Conferer.FromConfig.Snap ()
import qualified Snap.Http.Server.Config as Snap
import qualified Snap.Internal.Core as Snap
import           Data.Text (Text)

defaultConfig :: Snap.Config Snap.Snap ()
defaultConfig = Snap.defaultConfig

configWith :: [(Key, Text)] -> IO Config
configWith keyValues =
  emptyConfig 
  & addDefault "snap" defaultConfig
  & addSource (mkMapSource keyValues) 

spec :: Spec
spec = do
--   let defaultPort = getPort defaultSettings
--       defaultHost = getHost defaultSettings

  describe "fetching a snap configuration from a totally empty config" $ do
    it "throws an exception" $ do
      let config = emptyConfig
      getFromConfig @(Snap.Config Snap.Snap ()) "snap" config 
        `shouldThrow` anyException
  describe "fetching a snap configuration that has the default" $ do
    it "returns snap default config" $ do
      config <- configWith []
      fetchedValue <- getFromConfig @(Snap.Config Snap.Snap ()) "snap" config
      Snap.getPort fetchedValue
        `shouldBe` Snap.getPort (Snap.defaultConfig :: Snap.Config Snap.Snap ())
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- configWith [("snap.port", "5555")]
      fetchedValue <- getFromConfig @(Snap.Config Snap.Snap ()) "snap" config
      Snap.getPort fetchedValue
        `shouldBe` Just 5555
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <- addDefault "snap" (Snap.setPort 5555 defaultConfig)
        <$> configWith []
      fetchedValue <- getFromConfig @(Snap.Config Snap.Snap ()) "snap" config
      Snap.getPort fetchedValue
        `shouldBe` Just 5555
  describe "fetching a snap configuration overriding its port" $ do
    it "returns a snap config with its port set to the overriden one" $ do
      config <-
        emptyConfig 
        & addDefault "snap" (Snap.defaultConfig :: Snap.Config Snap.Snap Int)
        & addSource (mkMapSource [("snap.other", "55")]) 
      fetchedValue <- getFromConfig @(Snap.Config Snap.Snap Int) "snap" config
      Snap.getOther fetchedValue
        `shouldBe` Just 55

