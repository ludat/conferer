module Conferer.Provider.EnvSpec where

import           Test.Hspec
import qualified Data.Map as Map

import Conferer

fakeLookupEnv :: [(String, String)] -> LookupEnvFunc
fakeLookupEnv fakeEnv = \envName ->
  return $ Map.lookup envName $ Map.fromList fakeEnv

spec = do
  xdescribe "with an env config" $ do
    let
      p =
        mkEnvConfigProvider'
        (fakeLookupEnv
         [ ("TMUX","/tmp/tmux-1000/default,2822,0")
         , ("TMUX_PANE","%1")
         , ("TMUX_PLUGIN_MANAGER_PATH","/home/user/.tmux/plugins/")
         ])
        "TMUX"
    it "getting an existent key returns unwraps top level value (wihtout \
       \children)" $ do
      res <- p `getKeyInProvider` "."
      res `shouldBe` Just "/tmp/tmux-1000/default,2822,0"

    it "getting an existent key for a child gets that value" $ do
      res <- p `getKeyInProvider` "pane"
      res `shouldBe` Just "%1"

  -- TODO CONTINUE Here
    it "keys should always be consistent as to how the words are separated" $ do
      undefined
      res <- p `getKeyInProvider` "pane"
      res `shouldBe` Just "%1"
