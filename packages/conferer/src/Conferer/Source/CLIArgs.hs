-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- Command line arguments based source
module Conferer.Source.CLIArgs where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

import Conferer.Source
import qualified Conferer.Source.InMemory as InMemory
import Data.List (intercalate)


-- | This source provides keys from the command line arguments passed into
-- the program. It only accepts arguments with @--@ and an equals, for
-- example: @./awesomeapp --warp.port=5000@
data CLIArgsSource =
  CLIArgsSource
  { originalCliArgs :: RawCLIArgs
  , rawMap :: InMemory.RawInMemorySource
  } deriving (Show)

-- | Type alias for cli args
type RawCLIArgs = [String]

instance IsSource CLIArgsSource where
  getKeyInSource CLIArgsSource{..} key = do
    return $ InMemory.lookupKey key rawMap
  getSubkeysInSource CLIArgsSource{..} key = do
    return $ InMemory.subKeys key rawMap
  explainNotFound _ key =
    concat
    [ "Passing the cli arg: "
    , "--"
    , intercalate "." $ fmap (Text.unpack) $ rawKeyComponents key
    , "=\"the value\""
    ]
  explainSettedKey _ key =
    concat
    [ "cli param '--"
    , intercalate "." $ fmap (Text.unpack) $ rawKeyComponents key
    , "'"
    ]

-- | Create a 'SourceCreator' using 'fromEnv'
fromConfig :: SourceCreator
fromConfig = \_config ->
  fromEnv

-- | Create a 'Source' using 'fromArgs' but using real cli args
fromEnv :: IO Source
fromEnv = do
  args <- getArgs
  return $ fromArgs args

-- | Create a 'Source' using cli args passed as parameter
fromArgs :: RawCLIArgs -> Source
fromArgs originalCliArgs =
  let configMap = parseArgsIntoKeyValue originalCliArgs
      rawMap = InMemory.rawFromAssociations configMap
  in Source $ CLIArgsSource {..}

-- | Parse an argument list into a dictionary suitable for a 'Source'
parseArgsIntoKeyValue :: [String] -> [(Key, Text)]
parseArgsIntoKeyValue =
  fmap (\s ->
    let (k, rawValue) = Text.breakOn "=" s
    in case Text.uncons rawValue of
         Nothing -> (fromText k, "true")
         (Just ('=', v)) -> (fromText k, v)
         -- Since rawValue comes from breaking on "=" the first character
         -- should always be '=' or none at all
         (Just (_, _)) ->
            error $ unlines
              [ "'"
              , Text.unpack rawValue
              , "' should always start with '='"
              ]
    ) .
  mapMaybe (Text.stripPrefix "--") .
  takeWhile (/= "--") .
  fmap Text.pack
