module Conferer.Types where


import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Core interface for library provided configuration, basically consists of
--   getting a 'Key' and informing returning a maybe signaling the value and
--   if it's present in that specific provider
data Provider =
  Provider
  { getKeyInProvider :: Key -> IO (Maybe Text)
  }

-- | The way to index 'Provider's, basically list of names that will be adapted
--   to whatever the provider needs
newtype Key
  = Path { unKey :: [Text] }
  deriving (Show, Eq, Ord)

-- | Collapse a key into a textual representation
keyName :: Key -> Text
keyName = Text.intercalate "." . unKey

-- | Core type that the user of this library interact with, in the future it may
--   contain more this besides a list of providers
data Config =
  Config
  { providers :: [Provider]
  }

-- | The type for creating a provider given a 'Config', some providers require a
-- certain configuration to be initialized (for example: the redis provider
-- needs connection info to connect to the server)
type ProviderCreator = Config -> IO Provider

-- | Main typeclass for defining the way to get values from config, hiding the
-- 'Text' based nature of the 'Provider's
--
-- Here an error means that the value couldn't be parsed and that a reasonable
-- default was not possible.
class FetchFromConfig a where
  fetch :: Key -> Config -> IO (Either Text a)

instance IsString Key where
  fromString s = Path $ filter (/= mempty) $ Text.split (== '.') $ fromString s
