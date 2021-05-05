-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Some testing utilities
module Conferer.Test
  ( configWith
  ) where

import Conferer.Config
import Data.Text (Text)
import Data.Dynamic
import qualified Conferer.Source.Test as Test

-- | Create a Config mostly used for testing 'Conferer.FromConfig.FromConfig'
--   instances without having to create a full config
configWith :: [(Key, Dynamic)] -> [(Key, Text)] -> IO Config
configWith defaults keyValues =
  emptyConfig
  & addDefaults defaults
  & addSource (Test.fromConfig keyValues)
