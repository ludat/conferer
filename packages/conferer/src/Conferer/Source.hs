{-# LANGUAGE ExistentialQuantification #-}
module Conferer.Source
  ( Source(..)
  , IsSource(..)
  , SourceCreator
  , module Conferer.Key
  ) where

import           Conferer.Key
import           Conferer.Config.Internal.Types
import           Conferer.Source.Internal
