module Conferer.Source.ArgsSpec where

import           Test.Hspec

import           Conferer.Source
import           Conferer.Config.Internal
import           Conferer.Source.CLIArgs

spec :: Spec
spec = do
  describe "with a mapping source" $ do
    let mkConf args =
          mkStandaloneSource $ mkCLIArgsSource' args

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
