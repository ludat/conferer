module Conferer.Test where

import Conferer.Config
import Conferer.Source.Simple
import Data.Text (Text)
import Data.Dynamic

configWith :: [(Key, Dynamic)] -> [(Key, Text)] -> IO Config
configWith defaults keyValues =
  emptyConfig 
  & addDefaults defaults
  & addSource (mkMapSource keyValues)
