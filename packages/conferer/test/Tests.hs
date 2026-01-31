{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Tests where

import Test.Hspec
import Test.Hspec.Golden
import Test.Hspec.Core.Spec

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List (intercalate)
import System.FilePath ((</>), dropExtension)
import System.Directory (removeFile)
import Control.Monad (when)
import Control.Exception
import qualified Data.Text as Text

-- | Get the absolute path of the current module's source file
getCurrentModulePath :: Q Exp
getCurrentModulePath = do
  loc <- qLocation
  let path = loc_filename loc
  [| path |]


golden2
  :: HasCallStack
  => Bool       -- ^ Should override old value
  -> FilePath   -- ^ Path of current file
  -> String     -- ^ Test description
  -> IO String  -- ^ Content (@return content@ for pure functions)
  -> Spec
golden2 shouldOverride sourceFilePath description runAction = do
  pathBasedOnHspec <- intercalate "/" <$> drop 1 <$> (++ [description]) <$> getSpecDescriptionPath
  it description $ do
    result <- runAction
    let pathBasedOnCurrentFile = dropExtension sourceFilePath ++ ".golden"
    let path = pathBasedOnCurrentFile </> pathBasedOnHspec
    let goldenPath = path </> "golden"
    let actualPath = path </> "actual"

    when shouldOverride $ removeFile goldenPath

    pure $ Golden
      { output = result
      , encodePretty = show
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = goldenPath
      , actualFile = Just actualPath
      , failFirstTime = False
    }

betterGoldenOverride :: Q Exp
betterGoldenOverride = do
  loc <- qLocation
  let path = loc_filename loc
  [| golden2 True path |]

betterGolden :: Q Exp
betterGolden = do
  loc <- qLocation
  let path = (++ ".golden") $ dropExtension $ loc_filename loc
  [| golden2 False path |]


-- Helper functions

captureException :: IO a -> IO String
captureException action = do
  result <- try @SomeException action
  case result of
    Left e -> return $
      Text.unpack $
      fst $
      Text.breakOnEnd "HasCallStack backtrace" $
      Text.pack $
      displayException e
    Right _ -> fail "Expected an exception but got a successful result"
