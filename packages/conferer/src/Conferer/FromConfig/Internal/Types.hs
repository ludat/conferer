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

-- -- | Exception to show that a value couldn't be parsed properly
-- data ConfigParsingError =
--   ConfigParsingError Key Text TypeRep
--   deriving (Typeable, Eq)

instance Exception ConfigParsingError where
  displayException (ConfigParsingError key _value aTypeRep sourceIndex c) =
    concat
    [ "Failed to interpret "
    , explainSettedKey (configSources c !! sourceIndex) key
    , " as '"
    , show aTypeRep
    , "'"
    ]

-- instance Exception ConfigParsingError
-- instance Show ConfigParsingError where
--   show (ConfigParsingError key value aTypeRep) =
--     concat
--     [ "Couldn't parse value '"
--     , Text.unpack value
--     , "' from key '"
--     , show key
--     , "' as "
--     , show aTypeRep
--     ]

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
      sourcesExplanations =
        fmap (\s -> explainNotFound s $ head someKeys) $
        configSources config
    in unlines
    [ "Couldn't find a '" ++ show aTypeRep ++ "'."
    , ""
    , "You can set it by either:"
    ] ++ (intercalate "\n" $ fmap ("* " ++) $ sourcesExplanations)

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




-- -- | Helper function to throw 'ConfigParsingError'
-- throwConfigParsingError :: forall a b. (Typeable a) => Key -> Text -> IO b
-- throwConfigParsingError key text =
--   throwIO $ configParsingError @a  key text

-- -- | Helper function to create a 'ConfigParsingError'
-- configParsingError :: forall a. (Typeable a) => Key -> Text -> ConfigParsingError
-- configParsingError key text =
--   ConfigParsingError key text $ typeRep (Proxy :: Proxy a)

-- -- | Exception to show that some non optional 'Key' was missing while trying
-- -- to 'fetchFromConfig'
-- data MissingRequiredKey =
--   MissingRequiredKey [Key] TypeRep
--   deriving (Typeable, Eq)

-- instance Exception MissingRequiredKey
-- instance Show MissingRequiredKey where
--   show (MissingRequiredKey keys aTypeRep) =
--     concat
--     [ "Failed to get a '"
--     , show aTypeRep
--     , "' from keys: "
--     , Text.unpack
--       $ Text.intercalate ", "
--       $ fmap (Text.pack . show)
--       $ keys

--     ]

-- -- | Simplified helper function to throw a 'MissingRequiredKey'
-- throwMissingRequiredKey :: forall t a. Typeable t => Key -> IO a
-- throwMissingRequiredKey key =
--   throwMissingRequiredKeys @t [key]

-- -- | Simplified helper function to create a 'MissingRequiredKey'
-- missingRequiredKey :: forall t. Typeable t => Key -> MissingRequiredKey
-- missingRequiredKey key =
--   missingRequiredKeys @t [key]

-- -- | Helper function to throw a 'MissingRequiredKey'
-- throwMissingRequiredKeys :: forall t a. Typeable t => [Key] -> IO a
-- throwMissingRequiredKeys keys =
--   throwIO $ missingRequiredKeys @t keys

-- -- | Helper function to create a 'MissingRequiredKey'
-- missingRequiredKeys :: forall a. (Typeable a) => [Key] -> MissingRequiredKey
-- missingRequiredKeys keys =
--   MissingRequiredKey keys (typeRep (Proxy :: Proxy a))
