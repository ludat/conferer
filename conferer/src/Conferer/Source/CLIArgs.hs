module Conferer.Source.CLIArgs
  (
    -- * Command line arguments Source
    -- | This source provides keys from the command line arguments passed into
    -- the program. It only accepts arguments with @--@ and an equals, for
    -- example: @./awesomeapp --warp.port=5000@
    mkCLIArgsSource
    , mkCLIArgsSource'
    , parseArgsIntoKeyValue
  )
where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import           System.Environment (getArgs)

import Conferer.Types
import Conferer.Source.Simple


-- | Create a 'SourceCreator' for CLIArgs from a argument list
mkCLIArgsSource' :: [String] -> SourceCreator
mkCLIArgsSource' args = \config -> do
  let configMap = parseArgsIntoKeyValue args
  mkMapSource configMap config

-- | Same as 'mkCLIArgsSource'' but using 'getArgs' to provide the argument
-- list
mkCLIArgsSource :: SourceCreator
mkCLIArgsSource = \config -> do
  args <- getArgs
  mkCLIArgsSource' args config

-- | Parse an argument list into a dictionary suitable for a 'Source'
parseArgsIntoKeyValue :: [String] -> [(Key, Text)]
parseArgsIntoKeyValue =
  fmap (\(k, s) -> (fromString $ Text.unpack k, s)) .
  fmap (\s -> fmap (Text.drop 1) $ Text.breakOn "=" s) .
  mapMaybe (Text.stripPrefix "--") .
  takeWhile (/= "--") .
  fmap Text.pack
