{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Conferer.FromConfig.Aeson where

import Data.Aeson

import Conferer.FromConfig
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

instance FromConfig Value where
  fetchFromConfig key config = do
    rawAeson <- fetchFromConfig @Text key config
    case eitherDecodeStrict' @Value $ Text.encodeUtf8 rawAeson of
      Right value -> 
        return value
      Left _ -> 
        throwConfigParsingError @Value key rawAeson