-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Source for json config files using Aeson
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Conferer.FromConfig.Aeson where

import Data.Aeson

import Conferer.FromConfig
import qualified Data.Text.Encoding as Text

instance FromConfig Value where
  fromConfig = fetchFromConfigWith (decodeStrict' . Text.encodeUtf8)
