module Conferer.Core where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Function ((&))
import           Data.Either (either)
import           Control.Applicative ((<|>))

import           Conferer.Types

unsafeGetKey :: Key -> Config -> IO Text
unsafeGetKey k config =
  either (error . Text.unpack) id <$> getKey k config

getKey :: Key -> Config -> IO (Either Text Text)
getKey k config =
  foldr (<|>) (pure notFoundKey) $ map getFromProvider (providers config)
    where notFoundKey = Left ("Key '" <> keyName k <> "' was not found")
          getFromProvider provider = maybe notFoundKey Right <$> getKeyInProvider provider k

(/.) :: Key -> Key -> Key
parent /. child = Path (unKey parent ++ unKey child)

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
