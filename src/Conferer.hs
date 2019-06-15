module Conferer
  ( module Conferer.Provider.Env
  , module Conferer.Provider.Simple
  , module Conferer.Provider.Namespaced
  , module Conferer.Provider.JSON
  , module Conferer.Provider.Mapping
  , getKey
  , emptyConfig
  , addProvider
  -- , Config(..)
  , Key(..)
  , getKeyInProvider
  , (&)
  , mkStandaloneProvider
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Function ((&))

import           Conferer.Types
import           Conferer.Provider.Env
import           Conferer.Provider.Simple
import           Conferer.Provider.Namespaced
import           Conferer.Provider.JSON
import           Conferer.Provider.Mapping


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
