module Conferer.Source.CLIArgsSpec where

import Test.Hspec

import Conferer.Source
import Conferer.Source.CLIArgs
import Conferer.Config (emptyConfig)

spec :: Spec
spec = do
  describe "with a mapping source" $ do
    let mkConf args =
          mkCLIArgsSource' args emptyConfig

    it "gets a parameters with it's value if it starts with the right prefix" $ do
      c <- mkConf []
      res <- getKeyInSource c "some.key"
      res `shouldBe` Nothing

    it "with a value that begins with the right prefix it uses it" $ do
      c <- mkConf ["--some.key=value"]
      res <- getKeyInSource c "some.key"
      res `shouldBe` Just "value"

    it "with a reapeated value it uses the last one" $ do
      c <- mkConf ["--some.key=value", "--some.key=different value"]
      res <- getKeyInSource c "some.key"
      res `shouldBe` Just "different value"

    it "ignores values that don't start with the right prefix" $ do
      c <- mkConf ["some.key=value", "-some.key=value", "-Xsome.key=value", "some.key"]
      res <- getKeyInSource c "some.key"
      res `shouldBe` Nothing

    it "after encountering a -- it stops parsing parameters" $ do
      c <- mkConf ["--", "--some.key=value"]
      res <- getKeyInSource c "some.key"
      res `shouldBe` Nothing

    it "with passed flag without a value it returns true" $ do
      c <- mkConf ["--some.key"]
      res <- getKeyInSource c "some.key"
      res `shouldBe` Just "true"
