module Conferer.Provider.EnvSpec where

import           Test.Hspec
import qualified Data.Map as Map

import Conferer

fakeLookupEnv :: [(String, String)] -> LookupEnvFunc
fakeLookupEnv fakeEnv = \envName ->
  return $ Map.lookup envName $ Map.fromList fakeEnv

spec = do
  describe "with an env config" $ do
    let
      mkEnvConfig =
        emptyConfig
        & addProvider
        (mkEnvProvider'
         (fakeLookupEnv
          [ ("TMUX","/tmp/tmux-1000/default,2822,0")
          , ("TMUX_PANE","%1")
          , ("TMUX_PLUGIN_MANAGER_PATH","/home/user/.tmux/plugins/")
          ])
         "TMUX"
        )
    it "getting an existent key returns unwraps top level value (wihtout \
       \children)" $ do
      c <- mkEnvConfig
      res <- getKey "." c
      res `shouldBe` Right "/tmp/tmux-1000/default,2822,0"

    it "getting an existent key for a child gets that value" $ do
      c <- mkEnvConfig
      res <- getKey "pane" c
      res `shouldBe` Right "%1"

    it "keys should always be consistent as to how the words are separated" $ do
      c <- mkEnvConfig
      res <- getKey "pane" c
      res `shouldBe` Right "%1"
