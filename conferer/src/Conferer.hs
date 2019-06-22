module Conferer
  ( module Conferer.Core
  , module Conferer.Provider.Env
  , module Conferer.Provider.Simple
  , module Conferer.Provider.Namespaced
  , module Conferer.Provider.Mapping
  , module Conferer.Provider.CLIArgs
  , module Conferer.Provider.Null
  , defaultConfig
  , Key(..)
  , (&)
  ) where

import           Data.Text (Text)
import           Data.Function ((&))

import           Conferer.Core
import           Conferer.Types
import           Conferer.Provider.Env
import           Conferer.Provider.Simple
import           Conferer.Provider.Namespaced
import           Conferer.Provider.Mapping
import           Conferer.Provider.CLIArgs
import           Conferer.Provider.Null



defaultConfig :: Text -> IO Config
defaultConfig appName = do
  pure emptyConfig
  >>= addProvider (mkCLIArgsProvider)
  >>= addProvider (mkEnvProvider appName)

