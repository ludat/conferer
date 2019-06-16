module Conferer.Types where


import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text

data Provider =
  Provider
  { getKeyInProvider :: Key -> IO (Maybe Text)
  }

newtype Key
  = Path { unKey :: [Text] }
  deriving (Show, Eq, Ord)

keyName :: Key -> Text
keyName = Text.intercalate "." . unKey

data Config =
  Config
  { providers :: [Provider]
  }

type ProviderCreator = Config -> IO Provider

class FetchFromConfig a where
  fetch :: Key -> Config -> IO (Either Text a)

instance IsString Key where
  fromString s = Path $ filter (/= mempty) $ Text.split (== '.') $ fromString s

type Prefix = Text
