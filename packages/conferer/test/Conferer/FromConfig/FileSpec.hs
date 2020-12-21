{-# LANGUAGE TypeApplications #-}
module Conferer.FromConfig.FileSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended
import Conferer.FromConfig ()
import System.FilePath ((</>))
import Data.Text (pack)

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
          [ ("", pack $ "some" </> "file.png")
          ]
          []
          $ File $ "some" </> "file.png"

      context "with whole path in root and overriding extension" $ do
        ensureFetchParses @File
          [ ("", pack $ "some" </> "file.png")
          , ("extension", "jpg")
          ]
          []
          $ File $ "some" </> "file.jpg"

      context "with whole path in root and overriding extension and basename" $ do
        ensureFetchParses @File
          [ ("", pack $ "some" </> "file.png")
          , ("extension", "jpg")
          , ("basename", "somefile")
          ]
          []
          $ File $ "some" </> "somefile.jpg"

      context "with whole path in root and overriding dirname" $ do
        ensureFetchParses @File
          [ ("", pack $ "some" </> "file.png")
          , ("dirname", "dir")
          ]
          []
          $ File $ "dir" </> "file.png"

      context "with both filename and basename the most specific takes priority" $ do
        ensureFetchParses @File
          [ ("", pack $ "some" </> "file.png")
          , ("filename", "somefile.jpg")
          , ("basename", "coolfile")
          ]
          []
          $ File $ "some" </> "coolfile.jpg"

      context "with both filename and extension the most specific takes priority" $ do
        ensureFetchParses @File
          [ ("", pack $ "some" </> "file.png")
          , ("filename", "somefile.jpg")
          , ("extension", "gif")
          ]
          []
          $ File $ "some" </> "somefile.gif"

      context "without root path specificied, with subcomponents that complete a path" $ do
        ensureFetchParses @File
          [ ("filename", "somefile.jpg")
          , ("dirname", pack $ "some" </> "path")
          ]
          []
          $ File $ "some" </> "path" </> "somefile.jpg"
