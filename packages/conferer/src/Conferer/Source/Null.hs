-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Do nothing source
module Conferer.Source.Null where

import Conferer.Source

-- | Stub source that has no keys
data NullSource = NullSource
  { nullExplainNotFound :: Key -> String
  }

instance Show NullSource where
  show _ = "NullSource"

instance IsSource NullSource where
  getKeyInSource _source _key =
    return Nothing
  getSubkeysInSource _source _key =
    return []
  explainNotFound NullSource {..} = nullExplainNotFound
  explainSettedKey _source key =
    "Called explainSettedKey on NullSource with key " ++ show key ++ " , \
    \which is probably a bug in conferer (please report at https://github.com/ludat/conferer/issues)"
