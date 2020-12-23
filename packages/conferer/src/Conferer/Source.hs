-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Public API module for Source related features
module Conferer.Source
  ( Source(..)
  , IsSource(..)
  , SourceCreator
  , module Conferer.Key
  ) where

import Conferer.Key
import Conferer.Config.Internal.Types
import Conferer.Source.Internal
