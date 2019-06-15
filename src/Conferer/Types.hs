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


data Config =
  Config
  { providers :: [ConfigProvider]
  }

type ProviderCreator = Config -> IO ConfigProvider

class FromConfig a where
  fromConfig :: Config -> Either Text a

instance IsString Key where
  fromString s = Path $ filter (/= mempty) $ Text.split (== '.') $ fromString s

type Prefix = Text
