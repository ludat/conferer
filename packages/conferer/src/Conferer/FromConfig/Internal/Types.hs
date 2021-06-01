{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Conferer.FromConfig.Internal.Types where

import Control.Exception
import Conferer.Key (Key)
import Data.Text (Text)
import Data.Typeable
import Conferer.Config.Internal.Types
import Conferer.Source.Internal
import Data.List (intercalate)

type FailureExplanation = Text
type OriginalValue = Text

-- | Exception to show that a value couldn't be parsed properly
data ConfigParsingError =
  ConfigParsingError Key OriginalValue TypeRep Int Config
  deriving (Typeable, Show, Eq)

instance Exception ConfigParsingError where
  displayException (ConfigParsingError key _value aTypeRep sourceIndex c) =
    concat
    [ "Failed to interpret "
    , explainSettedKey (configSources c !! sourceIndex) key
    , " as '"
    , show aTypeRep
    , "'"
    ]

-- | Helper function to throw 'ConfigParsingError'
throwConfigParsingError :: forall a b. (Typeable a) => Key -> Text -> Int -> Config -> IO b
throwConfigParsingError key value sourceIndex config =
  throwIO $ configParsingError @a  key value sourceIndex config

-- | Helper function to create a 'ConfigParsingError'
configParsingError :: forall a. (Typeable a) => Key -> Text -> Int -> Config -> ConfigParsingError
configParsingError key value sourceIndex config =
  ConfigParsingError key value (typeRep (Proxy :: Proxy a)) sourceIndex config

-- | Exception to show that some non optional 'Key' was missing while trying
-- to 'fetchFromConfig'
data MissingRequiredKey =
  MissingRequiredKey [Key] TypeRep Config
  deriving (Typeable, Show, Eq)

instance Exception MissingRequiredKey where
  displayException (MissingRequiredKey someKeys aTypeRep config) =
    let
      explainSingleKey k =
        concat
        $ fmap (\source -> "* " ++ explainNotFound source k ++ "\n")
        $ configSources config
      keyExplanations =
        fmap explainSingleKey someKeys
    in unlines
    [ "Couldn't find a '" ++ show aTypeRep ++ "'."
    , ""
    , "You can set it by either:"
    ] ++ intercalate "\n" keyExplanations

-- | Simplified helper function to throw a 'MissingRequiredKey'
throwMissingRequiredKey :: forall t a. Typeable t => Key -> Config -> IO a
throwMissingRequiredKey key config =
  throwMissingRequiredKeys @t [key] config

-- | Simplified helper function to create a 'MissingRequiredKey'
missingRequiredKey :: forall t. Typeable t => Key -> Config -> MissingRequiredKey
missingRequiredKey key config =
  missingRequiredKeys @t [key] config

-- | Helper function to throw a 'MissingRequiredKey'
throwMissingRequiredKeys :: forall t a. Typeable t => [Key] -> Config -> IO a
throwMissingRequiredKeys keys config =
  throwIO $ missingRequiredKeys @t keys config

-- | Helper function to create a 'MissingRequiredKey'
missingRequiredKeys :: forall a. (Typeable a) => [Key] -> Config -> MissingRequiredKey
missingRequiredKeys keys config =
  MissingRequiredKey keys (typeRep (Proxy :: Proxy a)) config

