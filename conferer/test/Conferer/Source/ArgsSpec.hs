module Conferer.Source.ArgsSpec where

import           Test.Hspec

import           Conferer

spec :: Spec
spec = do
  describe "with a mapping source" $ do
    let mkConf args =
          emptyConfig
          & addSource
          (mkCLIArgsSource' args)

    it "gets a parameters with it's value if it starts with the right prefix" $ do
      c <- mkConf []
      res <- getKey "some.key" c
      res `shouldBe` Nothing

    it "with a value that begins with the right prefix it uses it" $ do
      c <- mkConf ["--some.key=value"]
      res <- getKey "some.key" c
      res `shouldBe` Just "value"

    it "with a reapeated value it uses the last one" $ do
      c <- mkConf ["--some.key=value", "--some.key=different value"]
      res <- getKey "some.key" c
      res `shouldBe` Just "different value"

    it "ignores values that don't start with the right prefix" $ do
      c <- mkConf ["some.key=value", "-some.key=value", "-Xsome.key=value", "some.key"]
      res <- getKey "some.key" c
      res `shouldBe` Nothing

    it "after encountering a -- it stops parsing parameters" $ do
      c <- mkConf ["--", "--some.key=value"]
      res <- getKey "some.key" c
      res `shouldBe` Nothing
