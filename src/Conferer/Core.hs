module Conferer.Core where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Function ((&))
import           Data.Either (either)

import           Conferer.Types

unsafeGetKey :: Key -> Config -> IO Text
unsafeGetKey k config =
  either (error . Text.unpack) id <$> getKey k config

getKey :: Key -> Config -> IO (Either Text Text)
getKey k config = do
  go $ providers config
  where
    go [] = return $ Left ("Key '" <> keyName k <> "' was not found")
    go (provider:providers) = do
      res <- getKeyInProvider provider k
      case res of
        Just t -> return $ Right t
        Nothing -> go providers

emptyConfig :: Config
emptyConfig = Config []

mkStandaloneProvider :: ProviderCreator -> IO ConfigProvider
mkStandaloneProvider mkProvider =
  mkProvider emptyConfig


addProvider :: ProviderCreator -> Config -> IO Config
addProvider mkProvider config = do
  newProvider <- mkProvider config
  return $
    Config
    { providers = providers config ++ [ newProvider ]
    }
