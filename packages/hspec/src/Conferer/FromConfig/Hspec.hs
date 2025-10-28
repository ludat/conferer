-- |
-- Copyright: (c) 2019 Lucas David Traverso
-- License: MPL-2.0
-- Maintainer: Lucas David Traverso <lucas6246@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for hspec
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Conferer.FromConfig.Hspec where

import Conferer.FromConfig

import Data.Text (toLower)
import Data.Dynamic (toDyn, Dynamic)

import qualified Test.Hspec.Core.Runner as Hspec
#if MIN_VERSION_hspec_core(2,8,0)
import qualified Test.Hspec.Core.Format as Hspec
#if MIN_VERSION_hspec_core(2,11,0)
import qualified Test.Hspec.Api.Formatters.V1 as FormattersV1
#else
import qualified Test.Hspec.Core.Formatters.V1 as FormattersV1
#endif
import qualified Test.Hspec.Core.Formatters.V2 as FormattersV2
#else
import qualified Test.Hspec.Core.Formatters as FormattersV1
#endif


#if MIN_VERSION_hspec_core(2,9,0)
instance FromConfig Hspec.UnicodeMode where
  fromConfig key config = do
    fetchFromConfigWith (
      (\case
        "auto" -> Just Hspec.UnicodeAuto
        "never" -> Just Hspec.UnicodeNever
        "always" -> Just Hspec.UnicodeAlways
        _ -> Nothing
      ) . toLower) key config

newtype AvailableFormatters =
  AvailableFormatters [(String, Hspec.FormatConfig -> IO Hspec.Format)]
#endif

#if MIN_VERSION_hspec_core(2,8,0)
newtype Format
  = Format { unFormat :: Hspec.FormatConfig -> IO Hspec.Format }

instance FromConfig Format where
  fromConfig key config = do
    formatter <- fetchFromConfigWith (
      (\case
        "silent" -> Just FormattersV2.silent
        "checks" -> Just FormattersV2.checks
        "specdoc" -> Just FormattersV2.specdoc
        "progress" -> Just FormattersV2.progress
        "failed-examples" -> Just FormattersV2.failed_examples
        "failed_examples" -> Just FormattersV2.failed_examples
        _ -> Nothing
      ) . toLower) key config
    pure $ Format $ FormattersV2.formatterToFormat formatter
#endif

instance FromConfig Hspec.ColorMode where
  fromConfig =
    fetchFromConfigWith $
    (\case
      "auto" -> Just Hspec.ColorAuto
      "never" -> Just Hspec.ColorNever
      "always" -> Just Hspec.ColorAlways
      _ -> Nothing
    ) . toLower

instance FromConfig FormattersV1.Formatter where
  fromConfig =
    fetchFromConfigWith $
    (\case
      "silent" -> Just FormattersV1.silent
#if MIN_VERSION_hspec_core(2,7,10)
      "checks" -> Just FormattersV1.checks
#endif
      "specdoc" -> Just FormattersV1.specdoc
      "progress" -> Just FormattersV1.progress
      "failed-examples" -> Just FormattersV1.failed_examples
      "failed_examples" -> Just FormattersV1.failed_examples
      _ -> Nothing
    ) . toLower

instance DefaultConfig Hspec.Config where
  configDef = Hspec.defaultConfig

-- | Deconstruct a 'Hspec.Config' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
desconstructHspecConfigToDefaults :: Hspec.Config -> [(Key, Dynamic)]
desconstructHspecConfigToDefaults Hspec.Config{..} =
  [ ("dryRun", toDyn configDryRun)
#if MIN_VERSION_hspec_core(2,9,0)
  , ("failFast", toDyn configFailFast)
#else
  , ("fastFail", toDyn configFastFail)
#endif
  , ("rerun", toDyn configRerun)
  , ("quickCheckMaxSuccess", toDyn configQuickCheckMaxSuccess)
  , ("quickCheckMaxDiscardRatio", toDyn configQuickCheckMaxDiscardRatio)
  , ("quickCheckMaxSize", toDyn configQuickCheckMaxSize)
  , ("smallCheckDepth", toDyn configSmallCheckDepth)
  , ("colorMode", toDyn configColorMode)
  , ("htmlOutput", toDyn configHtmlOutput)
#if !MIN_VERSION_hspec_core(2,11,0)
  , ("formatter", toDyn configFormatter)
#endif
  , ("rerunAllOnSuccess", toDyn configRerunAllOnSuccess)
  , ("filterPredicate", toDyn $ NotUserConfigurable configFilterPredicate)
#if !MIN_VERSION_hspec_core(2,8,0)
  , ("outputFile", toDyn configOutputFile)
#endif
#if MIN_VERSION_hspec_core(2,1,1)
  , ("skipPredicate", toDyn configSkipPredicate)
#endif
#if MIN_VERSION_hspec_core(2,1,9)
  , ("concurrentJobs", toDyn configConcurrentJobs)
#endif
#if MIN_VERSION_hspec_core(2,4,0)
  , ("ignoreConfigFile", toDyn configIgnoreConfigFile)
  , ("printCpuTime", toDyn configPrintCpuTime)
  , ("diff", toDyn configDiff)
#endif
#if MIN_VERSION_hspec_core(2,4,2)
  , ("failureReport", toDyn configFailureReport)
#endif
#if MIN_VERSION_hspec_core(2,7,0)
  , ("focusedOnly", toDyn configFocusedOnly)
  , ("failOnFocused", toDyn configFailOnFocused)
#endif
#if MIN_VERSION_hspec_core(2,7,3)
  , ("randomize", toDyn configRandomize)
#endif
#if MIN_VERSION_hspec_core(2,8,0)
  , ("printSlowItems", toDyn configPrintSlowItems)
  , ("quickCheckMaxShrinks", toDyn configQuickCheckMaxShrinks)
  , ("times", toDyn configTimes)
  , ("format", toDyn $ Format <$> configFormat)
#endif
#if MIN_VERSION_hspec_core(2,9,0)
  , ("unicodeMode", toDyn configUnicodeMode)
  , ("availableFormatters", toDyn $ NotUserConfigurable configAvailableFormatters)
#endif
#if MIN_VERSION_hspec_core(2,9,2)
  , ("prettyPrint", toDyn configPrettyPrint)
#endif
#if MIN_VERSION_hspec_core(2,10,0)
  , ("prettyPrintFunction", toDyn configPrettyPrintFunction)
#endif
#if MIN_VERSION_hspec_core(2,10,2)
  , ("failOnPending", toDyn configFailOnPending)
#endif
#if MIN_VERSION_hspec_core(2,10,5)
  , ("failOnEmpty", toDyn configFailOnEmpty)
#endif
#if MIN_VERSION_hspec_core(2,10,6)
  , ("diffContext", toDyn configDiffContext)
  , ("externalDiff", toDyn configExternalDiff)
#endif
#if MIN_VERSION_hspec_core(2,11,0)
  , ("failOnEmptyDescription", toDyn configFailOnEmptyDescription)
#endif
#if MIN_VERSION_hspec_core(2,11,2)
  , ("expertMode", toDyn configExpertMode)
#endif
#if MIN_VERSION_hspec_core(2,11,5)
  , ("formatException", toDyn configFormatException)
#endif
#if MIN_VERSION_hspec_core(2,11,8)
  , ("seed", toDyn configSeed)
#else
  , ("quickCheckSeed", toDyn configQuickCheckSeed)
#endif
#if MIN_VERSION_hspec_core(2,11,14)
  , ("annotations", toDyn $ NotUserConfigurable configAnnotations)
#endif
  ]

instance FromConfig Hspec.Config where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults
      desconstructHspecConfigToDefaults
      key originalConfig

    configDryRun <- fetchFromConfig (key /. "dryRun") config
#if MIN_VERSION_hspec_core(2,9,0)
    configFailFast <- fetchFromConfig (key /. "failFast") config
#else
    configFastFail <- fetchFromConfig (key /. "fastFail") config
#endif
    configRerun <- fetchFromConfig (key /. "rerun") config
    configQuickCheckMaxSuccess <- fetchFromConfig (key /. "quickCheckMaxSuccess") config
    configQuickCheckMaxDiscardRatio <- fetchFromConfig (key /. "quickCheckMaxDiscardRatio") config
    configQuickCheckMaxSize <- fetchFromConfig (key /. "quickCheckMaxSize") config
    configSmallCheckDepth <- fetchFromConfig (key /. "smallCheckDepth") config
    configColorMode <- fetchFromConfig (key /. "colorMode") config
    configHtmlOutput <- fetchFromConfig (key /. "htmlOutput") config
    configFormatter <- fetchFromConfig (key /. "formatter") config
    configRerunAllOnSuccess <- fetchFromConfig (key /. "rerunAllOnSuccess") config
    configFilterPredicate <- fmap unwrapNotConfigurable <$> fetchFromConfig (key /. "filterPredicate") config
#if !MIN_VERSION_hspec_core(2,8,0)
    NotUserConfigurable configOutputFile <- fetchFromConfig (key /. "outputFile") config
#endif
#if MIN_VERSION_hspec_core(2,1,1)
    NotUserConfigurable configSkipPredicate <- fetchFromConfig (key /. "skipPredicate") config
#endif
#if MIN_VERSION_hspec_core(2,1,9)
    configConcurrentJobs <- fetchFromConfig (key /. "concurrentJobs") config
#endif
#if MIN_VERSION_hspec_core(2,4,0)
    configIgnoreConfigFile <- fetchFromConfig (key /. "ignoreConfigFile") config
    configPrintCpuTime <- fetchFromConfig (key /. "printCpuTime") config
    configDiff <- fetchFromConfig (key /. "diff") config
#endif
#if MIN_VERSION_hspec_core(2,4,2)
    configFailureReport <- fetchFromConfig (key /. "failureReport") config
#endif
#if MIN_VERSION_hspec_core(2,7,0)
    configFocusedOnly <- fetchFromConfig (key /. "focusedOnly") config
    configFailOnFocused <- fetchFromConfig (key /. "failOnFocused") config
#endif
#if MIN_VERSION_hspec_core(2,7,3)
    configRandomize <- fetchFromConfig (key /. "randomize") config
#endif
#if MIN_VERSION_hspec_core(2,8,0)
    configPrintSlowItems <- fetchFromConfig (key /. "printSlowItems") config
    configQuickCheckMaxShrinks <- fetchFromConfig (key /. "quickCheckMaxShrinks") config
    configTimes <- fetchFromConfig (key /. "times") config
    configFormat <- fmap unFormat <$> fetchFromConfig (key /. "format") config
#endif
#if MIN_VERSION_hspec_core(2,9,0)
    configUnicodeMode <- fetchFromConfig (key /. "unicodeMode") config
    NotUserConfigurable configAvailableFormatters <- fetchFromConfig (key /. "availableFormatters") config
#endif
#if MIN_VERSION_hspec_core(2,9,2)
    configPrettyPrint <- fetchFromConfig (key /. "prettyPrint") config
#endif
#if MIN_VERSION_hspec_core(2,10,0)
    NotUserConfigurable configPrettyPrintFunction <- fetchFromConfig (key /. "prettyPrintFunction") config
#endif
#if MIN_VERSION_hspec_core(2,10,2)
    configFailOnPending <- fetchFromConfig (key /. "failOnPending") config
#endif
#if MIN_VERSION_hspec_core(2,10,5)
    configFailOnEmpty <- fetchFromConfig (key /. "failOnEmpty") config
#endif
#if MIN_VERSION_hspec_core(2,10,6)
    configDiffContext <- fetchFromConfig (key /. "diffContext") config
    NotUserConfigurable configExternalDiff <- fetchFromConfig (key /. "externalDiff") config
#endif
#if MIN_VERSION_hspec_core(2,11,0)
    configFailOnEmptyDescription <- fetchFromConfig (key /. "failOnEmptyDescription") config
#endif
#if MIN_VERSION_hspec_core(2,11,2)
    configExpertMode <- fetchFromConfig (key /. "expertMode") config
#endif
#if MIN_VERSION_hspec_core(2,11,5)
    NotUserConfigurable configFormatException <- fetchFromConfig (key /. "formatException") config
#endif
#if MIN_VERSION_hspec_core(2,11,8)
    configSeed <- fetchFromConfig (key /. "seed") config
    let configQuickCheckSeed = Nothing
#else
    configQuickCheckSeed <- fetchFromConfig (key /. "quickCheckSeed") config
#endif
#if MIN_VERSION_hspec_core(2,11,10)
    NotUserConfigurable configAnnotations <- fetchFromConfig (key /. "annotations") config
#endif
    pure Hspec.Config{..}
