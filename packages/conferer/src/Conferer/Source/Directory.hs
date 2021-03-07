-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Directory based source

module Conferer.Source.Directory where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

import Conferer.Source
import qualified Conferer.Source.InMemory as InMemory


-- | This source provides keys from the command line arguments passed into
-- the program. It only accepts arguments with @--@ and an equals, for
-- example: @./awesomeapp --warp.port=5000@
data DirectorySource =
  DirectorySource
  { originalCliArgs :: RawCLIArgs
  , innerSource :: Source
  } deriving (Show)

-- | Type alias for cli args
type RawCLIArgs = [String]

instance IsSource DirectorySource where
  getKeyInSource DirectorySource{..} key = do
    getKeyInSource innerSource key
  getSubkeysInSource DirectorySource{..} key = do
    getSubkeysInSource innerSource key


-- | Create a 'SourceCreator' using 'fromEnv'
fromConfig :: SourceCreator
fromConfig = \_config ->
  undefined