{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.FileSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended
import Conferer.FromConfig ()

spec :: Spec
spec = do
  context "File fromConfig" $ do
    describe "fetching a File from config" $ do
      ensureFetchThrows @File [] [] $
        aMissingRequiredKeys @String
          [ "some.key"
          , "some.key.extension"
          , "some.key.dirname"
          , "some.key.basename"
          , "some.key.filename"
          ]

      ensureFetchThrows @File [] [("", toDyn False)] $
        aTypeMismatchWithDefaultError @String "some.key" False
      context "with whole path in root" $ do
        ensureFetchParses @File
          [ ("", "/some/path/to/file.png")
          ]
          []
          "/some/path/to/file.png"

      context "with whole path in root and overriding extension" $ do
        ensureFetchParses @File
          [ ("", "/some/file.png")
          , ("extension", "jpg")
          ]
          []
          "/some/file.jpg"

      context "with whole path in root and overriding extension and basename" $ do
        ensureFetchParses @File
          [ ("", "/some/file.png")
          , ("extension", "jpg")
          , ("basename", "somefile")
          ]
          []
          "/some/somefile.jpg"

      context "with whole path in root and overriding dirname" $ do
        ensureFetchParses @File
          [ ("", "/some/file.png")
          , ("dirname", "dir")
          ]
          []
          "dir/file.png"

      context "with both filename and basename the most specific takes priority" $ do
        ensureFetchParses @File
          [ ("", "/some/file.png")
          , ("filename", "somefile.jpg")
          , ("basename", "coolfile")
          ]
          []
          "/some/coolfile.jpg"

      context "with both filename and extension the most specific takes priority" $ do
        ensureFetchParses @File
          [ ("", "/some/file.png")
          , ("filename", "somefile.jpg")
          , ("extension", "gif")
          ]
          []
          "/some/somefile.gif"

      context "without root path specificied, with subcomponents that complete a path" $ do
        ensureFetchParses @File
          [ ("filename", "somefile.jpg")
          , ("dirname", "/some/path")
          ]
          []
          "/some/path/somefile.jpg"
