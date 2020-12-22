-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Internal module for Key related features
{-# LANGUAGE ExistentialQuantification #-}
module Conferer.Source.Internal where

import Data.Text (Text)
import Conferer.Key (Key)

-- | Concrete type for 'IsSource'
data Source = forall s. (IsSource s, Show s) => Source s

instance Show Source where
  show (Source s) = "Source " ++ show s

-- | Main interface for interacting with external systems that provide configuration
-- which will be used by 'Conferer.FromConfig.FromConfig' to fetch values.
class IsSource s where
  -- | This function is used by the 'Conferer.Config.Config' to get values from this
  -- 'Source'.
  getKeyInSource :: s ->  Key -> IO (Maybe Text)
  -- | This function is used by the 'Conferer.Config.Config' to list possible values
  -- from the 'Source' that if the user 'getKeyInSource', it will be found.
  getSubkeysInSource :: s -> Key -> IO [Key]

instance IsSource Source where
  getKeyInSource (Source source) =
    getKeyInSource source
  getSubkeysInSource (Source source) =
    getSubkeysInSource source
