module Conferer.Test where

import Conferer.Config
import qualified Conferer.Source.InMemory as InMemory
import Data.Text (Text)
import Data.Dynamic

configWith :: [(Key, Dynamic)] -> [(Key, Text)] -> IO Config
configWith defaults keyValues =
  emptyConfig 
  & addDefaults defaults
  & addSource (InMemory.fromConfig keyValues)
