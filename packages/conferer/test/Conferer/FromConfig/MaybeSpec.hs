
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conferer.FromConfig.MaybeSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended

spec :: Spec
spec = do
  describe "fetching a Maybe from config" $ do
    context "when the key is there and has a valid value" $ do
      ensureFetchParses @(Maybe Int) [("", "7")] [] $ Just 7
    context "when the key is missing" $ do
      ensureFetchParses @(Maybe Int) [] [] Nothing
    context "when the key is there but has a wrong value" $ do
      ensureFetchThrows @(Maybe Int) [("", "Bleh")] [] $ configParsingError @Int "some.key" "Bleh" 0 emptyConfig
    context "with default of the inner type" $ do
      ensureFetchParses @(Maybe Int) [] [("", toDyn @Int 7)] $ Just 7
    context "with default of a Just inner type" $ do
      ensureFetchParses @(Maybe Int) [] [("", toDyn @(Maybe Int) $ Just 7)] $ Just 7
    context "with default of a Nothing and no configuration" $ do
      ensureFetchParses @(Maybe Int) [] [("", toDyn @(Maybe Int) $ Nothing)] $ Nothing