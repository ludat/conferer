module Conferer.Provider.CLIArgs
  (
    -- * Command line arguments Provider
    -- | This provider provides keys from the command line arguments passed into
    -- the program. It only accepts arguments with @--@ and an equals, for
    -- example: @./awesomeapp --warp.port=5000@
    mkCLIArgsProvider
    , mkCLIArgsProvider'
    , parseArgsIntoKeyValue
  )
where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import           System.Environment (getArgs)

import Conferer.Types
import Conferer.Provider.Simple


-- | Create a 'ProviderCreator' for CLIArgs from a argument list
mkCLIArgsProvider' :: [String] -> ProviderCreator
mkCLIArgsProvider' args = \config -> do
  let configMap = parseArgsIntoKeyValue args
  mkMapProvider configMap config

-- | Same as 'mkCLIArgsProvider'' but using 'getArgs' to provide the argument
-- list
mkCLIArgsProvider :: ProviderCreator
mkCLIArgsProvider = \config -> do
  args <- getArgs
  mkCLIArgsProvider' args config

-- | Parse an argument list into a dictionary suitable for a 'Provider'
parseArgsIntoKeyValue :: [String] -> [(Key, Text)]
parseArgsIntoKeyValue =
  fmap (\(k, s) -> (fromString $ Text.unpack k, s)) .
  fmap (\s -> fmap (Text.drop 1) $ Text.breakOn "=" s).
  mapMaybe (Text.stripPrefix "--") .
  takeWhile (/= "--") .
  fmap Text.pack
