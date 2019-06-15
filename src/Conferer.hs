module Conferer
  ( module Conferer.Core
  , module Conferer.Provider.Env
  , module Conferer.Provider.Simple
  , module Conferer.Provider.Namespaced
  , module Conferer.Provider.JSON
  , module Conferer.Provider.Mapping
  , module Conferer.Provider.CLIArgs
  , defaultConfig
  , Key(..)
  , (&)
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Function ((&))
import           Data.Either (either)

import           Conferer.Core
import           Conferer.Types
import           Conferer.Provider.Files
import           Conferer.Provider.Env
import           Conferer.Provider.Simple
import           Conferer.Provider.Namespaced
import           Conferer.Provider.JSON
import           Conferer.Provider.Mapping
import           Conferer.Provider.CLIArgs



defaultConfig :: Text -> IO Config
defaultConfig appName = do
  pure emptyConfig
  >>= addProvider (mkCLIArgsProvider)
  >>= addProvider (mkEnvConfigProvider appName)
  >>= addProvider (mkJsonConfigProvider)
