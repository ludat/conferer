module Conferer.Source.EnvSpec where

import           Test.Hspec

import           Conferer.Source
import           Conferer.Config.Internal

import           Conferer.Source.Env

spec :: Spec
spec = do
  describe "with an env config" $ do
    let
      mkEnvConfig envVars =
        mkStandaloneSource $
          mkEnvSource' envVars "APP"

    it "getting an existent key returns unwraps top level value (wihtout \
       \children)" $ do
      c <- mkEnvConfig ([ ("APP_THING","thing")
            , ("APP","root")
            , ("APP_PG_HOST","some host")
            , ("APP_PG_PORT","some port")
            , ("SOMETHING_ELSE","else")
            ])
      res <- getKeyInSource c ""
      res `shouldBe` Just "root"

    it "getting an existent key for a child gets that value" $ do
      c <- mkEnvConfig (
            [ ("APP_THING","thing")
            ])
      res <- getKeyInSource c "thing"
      res `shouldBe` Just "thing"

    it "getting an existent key for a child gets that value" $ do
      c <- mkEnvConfig (
            [ ("APP_PG_HOST","some host")
            , ("APP_PG_PORT","some port")
            ])
      res <- getSubkeysInSource c "pg"
      res `shouldBe` ["pg.host", "pg.port"]

    it "listing the keys under the root list everything including ." $ do
      c <- mkEnvConfig (
            [ ("APP","root")
            , ("SOMETHING_ELSE","else")
            ])
      res <- getSubkeysInSource c ""
      res `shouldBe` [""]
