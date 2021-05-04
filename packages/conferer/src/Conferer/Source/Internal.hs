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
  -- | This function is used to explain the user how to set this 'Key' using this
  -- 'Source'.
  --
  -- It is used as: "You can set the key by " '++' exaplainNotFound source key
  explainNotFound :: s -> Key -> String
  -- | This function is used to reference a 'Key' without exposing 'Key's to the user.
  --
  -- It is used as: "I tried to read " '++' explainSettedKey source key ++ " as an
  -- Int but if failed"
  explainSettedKey :: s -> Key -> String

instance IsSource Source where
  getKeyInSource (Source source) =
    getKeyInSource source
  getSubkeysInSource (Source source) =
    getSubkeysInSource source
  explainNotFound (Source source) =
    explainNotFound source
  explainSettedKey (Source source) =
    explainSettedKey source
