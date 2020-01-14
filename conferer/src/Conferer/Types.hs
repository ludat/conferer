{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Conferer.Types where


import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Generics

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
  , defaults :: Map Key Text
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
  default fetch :: (Generic a, GFetchFromConfig (Rep a)) => Key -> Config -> IO (Either Text a)
  fetch a b = fmap to <$> fetch' a b

class GFetchFromConfig f where
  fetch' :: Key -> Config -> IO (Either Text (f a))

instance GFetchFromConfig b => GFetchFromConfig (D1 a b) where
  fetch' key config =
    fmap M1 <$> fetch' @b key config

instance GFetchFromConfig b => GFetchFromConfig (C1 a b) where
  fetch' key config =
    fmap M1 <$> fetch' @b key config

instance (GFetchFromConfig b, Selector a) => GFetchFromConfig (S1 a b) where
  fetch' key config =
    let 
      lele = selName @a undefined
    in fmap M1 <$> fetch' @b (Path $ unKey key ++ [Text.pack lele]) config

instance FetchFromConfig a => GFetchFromConfig (Rec0 a) where
  fetch' key config =
    fmap K1 <$> fetch @a key config

instance (GFetchFromConfig a, GFetchFromConfig b) => GFetchFromConfig (a :*: b) where
  fetch' key config =
    fetch' @a key config
      >>= \case
        Left x -> return $ Left x
        Right c1 ->
          fetch' @b key config
          >>= \case
            Left x -> return $ Left x
            Right c2 ->
              return $ Right (c1 :*: c2)

-- instance (FetchFromConfig b) => GFetchFromConfig (K1 a b) where
--   fetch' key config =
--     fmap K1 <$> fetch @b key config

instance IsString Key where
  fromString s = Path $ filter (/= mempty) $ Text.split (== '.') $ fromString s
