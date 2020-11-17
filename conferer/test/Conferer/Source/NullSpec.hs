module Conferer.Source.NullSpec where

import           Test.Hspec

import           Conferer.Source
import           Conferer.Config.Internal
import           Conferer.Source.Null

spec :: Spec
spec = do
  it "always fails to get a key" $ do
    c <- mkStandaloneSource mkNullSource
    res <- getKeyInSource c "some.key"
    res `shouldBe` Nothing
