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
import qualified Test.Hspec.Core.Formatters as Hspec

instance FromConfig Hspec.ColorMode where
  fromConfig =
    fetchFromConfigWith $
    (\case
      "auto" -> Just Hspec.ColorAuto
      "never" -> Just Hspec.ColorNever
      "always" -> Just Hspec.ColorAlways
      _ -> Nothing
    ) . toLower

instance FromConfig Hspec.Formatter where
  fromConfig =
    fetchFromConfigWith $
    (\case
      "silent" -> Just Hspec.silent
#if MIN_VERSION_hspec_core(2,7,10)
      "checks" -> Just Hspec.checks
#endif
      "specdoc" -> Just Hspec.specdoc
      "progress" -> Just Hspec.progress
      "failed-examples" -> Just Hspec.failed_examples
      "failed_examples" -> Just Hspec.failed_examples
      _ -> Nothing
    ) . toLower

instance DefaultConfig Hspec.Config where
  configDef = Hspec.defaultConfig

-- | Deconstruct a 'Hspec.Config' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
desconstructHspecConfigToDefaults :: Hspec.Config -> [(Key, Dynamic)]
desconstructHspecConfigToDefaults Hspec.Config{..} =
  [ ("dryRun", toDyn configDryRun)
  , ("fastFail", toDyn configFastFail)
  , ("rerun", toDyn configRerun)
  , ("quickCheckMaxSuccess", toDyn configQuickCheckMaxSuccess)
  , ("quickCheckMaxDiscardRatio", toDyn configQuickCheckMaxDiscardRatio)
  , ("quickCheckMaxSize", toDyn configQuickCheckMaxSize)
  , ("quickCheckSeed", toDyn configQuickCheckSeed)
  , ("smallCheckDepth", toDyn configSmallCheckDepth)
  , ("colorMode", toDyn configColorMode)
  , ("htmlOutput", toDyn configHtmlOutput)
  , ("formatter", toDyn configFormatter)
  , ("rerunAllOnSuccess", toDyn configRerunAllOnSuccess)
  , ("outputFile", toDyn configOutputFile)
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
  ]

instance FromConfig Hspec.Config where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults
      desconstructHspecConfigToDefaults
      key originalConfig

    configDryRun <- fetchFromConfig (key /. "dryRun") config
    configFastFail <- fetchFromConfig (key /. "fastFail") config
    configRerun <- fetchFromConfig (key /. "rerun") config
    configQuickCheckMaxSuccess <- fetchFromConfig (key /. "quickCheckMaxSuccess") config
    configQuickCheckMaxDiscardRatio <- fetchFromConfig (key /. "quickCheckMaxDiscardRatio") config
    configQuickCheckMaxSize <- fetchFromConfig (key /. "quickCheckMaxSize") config
    configQuickCheckSeed <- fetchFromConfig (key /. "quickCheckSeed") config
    configSmallCheckDepth <- fetchFromConfig (key /. "smallCheckDepth") config
    configColorMode <- fetchFromConfig (key /. "colorMode") config
    configHtmlOutput <- fetchFromConfig (key /. "htmlOutput") config
    configFormatter <- fetchFromConfig (key /. "formatter") config
    configRerunAllOnSuccess <- fetchFromConfig (key /. "rerunAllOnSuccess") config
    configFilterPredicate <- fetchFromConfig (key /. "filterPredicate") config
    configOutputFile <- fetchFromConfig (key /. "outputFile") config
#if MIN_VERSION_hspec_core(2,1,1)
    configSkipPredicate <- fetchFromConfig (key /. "skipPredicate") config
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
    pure Hspec.Config{..}
