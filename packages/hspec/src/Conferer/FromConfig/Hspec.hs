{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Conferer.FromConfig.Hspec
  (
  -- * How to use this
  -- | FromConfig instance for snap server configuration
  --
  -- @
  -- import Conferer
  -- import Conferer.FromConfig.Snap ()
  --
  -- main = do
  --   config <- 'defaultConfig' \"awesomeapp\"
  --   snapConfig <- 'fetchFromConfig' \"snap\" config
  -- @
  --
  -- * Internal utility functions
  -- | These may be useful for someone but are subject to change at any point so
  -- use with care
  ) where

import Conferer.FromConfig

import Data.Text (toLower)

import qualified Test.Hspec.Core.Runner as Hspec
import qualified Test.Hspec.Core.Formatters as Hspec
import Data.Dynamic (toDyn, Dynamic)

instance FromConfig Hspec.ColorMode where
  fetchFromConfig =
    fetchFromConfigWith $
    (\case
      "auto" -> Just Hspec.ColorAuto
      "never" -> Just Hspec.ColorNever
      "always" -> Just Hspec.ColorAlways
      _ -> Nothing
    ) . toLower

instance FromConfig Hspec.Formatter where
  fetchFromConfig =
    fetchFromConfigWith $
    (\case
      "silent" -> Just Hspec.silent
      "specdoc" -> Just Hspec.specdoc
      "progress" -> Just Hspec.progress
      "failed_examples" -> Just Hspec.failed_examples
      _ -> Nothing
    ) . toLower

instance DefaultConfig Hspec.Config where
  configDef = Hspec.defaultConfig

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
  ]

instance FromConfig Hspec.Config where
  fetchFromConfig key originalConfig = do
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
    pure Hspec.Config{..}
