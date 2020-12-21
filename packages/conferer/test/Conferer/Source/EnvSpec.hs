module Conferer.Source.EnvSpec where

import Test.Hspec
import Test.QuickCheck

import Conferer.Source
import Conferer.Source.Env

spec :: Spec
spec = do
  describe "with an env source" $ do
    let
      mkEnvConfig envVars =
        return $ fromEnvList envVars "APP"

    it "getting an existent key returns unwraps top level value (without \
       \children)" $ do
      c <- mkEnvConfig
            [ ("APP_THING","thing")
            , ("APP","root")
            , ("APP_PG_HOST","some host")
            , ("APP_PG_PORT","some port")
            , ("SOMETHING_ELSE","else")
            ]
      res <- getKeyInSource c ""
      res `shouldBe` Just "root"

    it "getting an existent key for a child gets that value" $ do
      c <- mkEnvConfig
            [ ("APP_THING","thing")
            ]
      res <- getKeyInSource c "thing"
      res `shouldBe` Just "thing"

    it "listing some keys returns all the right values" $ do
      c <- mkEnvConfig
            [ ("APP_PG_HOST","some host")
            , ("APP_PG_PORT","some port")
            ]
      res <- getSubkeysInSource c "pg"
      res `shouldBe` ["pg.host", "pg.port"]

    it "listing the keys under the root list everything excluding ." $ do
      c <- mkEnvConfig
            [ ("APP_SOME","value")
            , ("APP","value")
            , ("SOMETHING_ELSE","else")
            ]
      res <- getSubkeysInSource c ""
      res `shouldBe` ["some"]

    it "key to env var and back be identity" $ property $
      forAll
        (elements ["a", "a.b", "", "a.b.c", "UPPER"])
        $ \k ->
            envVarToKey "theapp" (keyToEnvVar "theapp" k)
              == Just k

