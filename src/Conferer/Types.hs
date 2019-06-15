module Conferer.Types where


import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text

data ConfigProvider =
  ConfigProvider
  { getKeyInProvider :: Key -> IO (Maybe Text)
  }

newtype Key
  = Path { unKey :: [Text] }
  deriving (Show, Eq, Ord)

keyName :: Key -> Text
keyName = Text.intercalate "." . unKey

data Config =
  Config
  { providers :: [ConfigProvider]
  }

type ProviderCreator = Config -> IO ConfigProvider

class FetchFromConfig a where
  fetch :: Key -> Config -> IO (Either Text a)

instance IsString Key where
  fromString s = Path $ filter (/= mempty) $ Text.split (== '.') $ fromString s

type Prefix = Text
