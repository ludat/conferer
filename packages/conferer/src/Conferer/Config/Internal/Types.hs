-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: unstable
-- Portability: portable
--
-- Core types for Config (here because of depedency cycles)
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Conferer.Config.Internal.Types where

import Data.Map (Map)
import Conferer.Key (Key)
import Data.Dynamic ( Dynamic )
import Conferer.Source.Internal (Source)
import Data.Text (Text)
import Data.Void
import Data.Kind

-- | This type acts as the entry point for most of the library, it's main purpouse
--   is to expose a uniform interface into multiple configuration sources (such as
--   env vars, cli args, and many others including use defined ones using the
--   'Source' interface)
data Config =
  Config
  { configSources :: [Source]
  , configDefaults :: Map Key [Dynamic]
  , configKeyMappings :: [(Key, Key)]
  } deriving (Show)

-- | Result of a key lookup in a 'Config'
--
-- This is indexed by 'LookupTarget' to indicate where we are looking for values
-- and some invariants (if you look for defaults you'll never get things from a
-- 'Source)
--
-- Possible valures are:
--   * 'BothSourcesAndDefaults' a: All constructors are possible, type parameter
--       indicates the type of the default to look for.
--   * 'OnlyDefaultsAs' a: 'FoundInSources' constructor is not possible, type
--       parameter indicates the type of the default to look for
--   * 'OnlySources': 'FoundInDefaults' constructor is not possible, type of
--       of the result is always 'Text'
data KeyLookupResult lookupTarget
  = MissingKey (MissingC lookupTarget) [Key]
  | FoundInSources (SourcesC lookupTarget) Key
  | FoundInDefaults (DefaultsC lookupTarget) Key

-- | Target of a lookup that the 'KeyLookupResult' is parameterized on
-- to indicate where the value was looked for in.
data LookupTarget a
 = BothSourcesAndDefaults a
 -- ^ Look in both Sources and defaults (this is the most common)
 -- its type parameter indicates the type of the default
 | OnlySources
 -- ^ Look only in the defaults ('FoundInDefaults' is not possible)
 | OnlyDefaultsAs a
 -- ^ Look only in the sources ('FoundInSources' is not possible)
 -- its type parameter indicates the type of the default

-- | Modifier type family for the 'MissingKey' constructor.
type family MissingC (a :: LookupTarget Type) where
  MissingC ('BothSourcesAndDefaults a) = ()
  MissingC 'OnlySources = ()
  MissingC ('OnlyDefaultsAs _) = ()

-- | Modifier type family for the 'FoundInSources' constructor.
type family SourcesC (a :: LookupTarget Type) where
  SourcesC ('BothSourcesAndDefaults a) = Text
  SourcesC 'OnlySources = Text
  SourcesC ('OnlyDefaultsAs _) = Void

-- | Modifier type family for the 'FoundInDefaults' constructor.
type family DefaultsC (a :: LookupTarget Type) where
  DefaultsC ('BothSourcesAndDefaults a) = a
  DefaultsC 'OnlySources = Void
  DefaultsC ('OnlyDefaultsAs a) = a

deriving instance
    ( Eq a, a ~ MissingC lookupTarget
    , Eq b, b ~ DefaultsC lookupTarget
    , Eq c, c ~ SourcesC lookupTarget
    )
  => Eq (KeyLookupResult lookupTarget)

deriving instance
    ( Show a, a ~ MissingC lookupTarget
    , Show b, b ~ DefaultsC lookupTarget
    , Show c, c ~ SourcesC lookupTarget
    )
  => Show (KeyLookupResult lookupTarget)

-- | The type for creating a source given a 'Config', some sources require a
-- certain configuration to be initialized (for example: the redis source
-- needs connection info to connect to the server)
type SourceCreator = Config -> IO Source
