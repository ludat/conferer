{-# LANGUAGE ExistentialQuantification #-}
module Conferer.Source.Internal (Source(..), IsSource(..)) where

import           Data.Text (Text)
import           Conferer.Key (Key)


data Source = forall s. (IsSource s, Show s) => Source s

instance Show Source where
  show (Source s) = "Source " ++ show s

class IsSource s where
  getKeyInSource :: s ->  Key -> IO (Maybe Text)
  getSubkeysInSource :: s -> Key -> IO [Key]

instance IsSource Source where
  getKeyInSource (Source source) =
    getKeyInSource source
  getSubkeysInSource (Source source) =
    getSubkeysInSource source
