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
  --   snapConfig <- 'getFromConfig' \"snap\" config
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

--   configDef = Hspec.defaultConfig

instance FromConfig Hspec.Config where
  fetchFromConfig key config = do
    Hspec.Config{..} <- fetchFromDefaults key config

    configDryRun <- getFromConfigWithDefault (key /. "dryRun") config configDryRun
    configFastFail <- getFromConfigWithDefault (key /. "fastFail") config configFastFail
    configRerun <- getFromConfigWithDefault (key /. "rerun") config configRerun
    configQuickCheckMaxSuccess <- getFromConfigWithDefault (key /. "quickCheckMaxSuccess") config configQuickCheckMaxSuccess
    configQuickCheckMaxDiscardRatio <- getFromConfigWithDefault (key /. "quickCheckMaxDiscardRatio") config configQuickCheckMaxDiscardRatio
    configQuickCheckMaxSize <- getFromConfigWithDefault (key /. "quickCheckMaxSize") config configQuickCheckMaxSize
    configQuickCheckSeed <- getFromConfigWithDefault (key /. "quickCheckSeed") config configQuickCheckSeed
    configSmallCheckDepth <- getFromConfigWithDefault (key /. "smallCheckDepth") config configSmallCheckDepth
    configColorMode <- getFromConfigWithDefault (key /. "colorMode") config configColorMode
    configHtmlOutput <- getFromConfigWithDefault (key /. "htmlOutput") config configHtmlOutput
    configFormatter <- getFromConfigWithDefault (key /. "formatter") config configFormatter
#if MIN_VERSION_hspec_core(2,1,1)
    configSkipPredicate <- getFromConfigWithDefault (key /. "skipPredicate") config configSkipPredicate
#endif
#if MIN_VERSION_hspec_core(2,1,9)
    configConcurrentJobs <- getFromConfigWithDefault (key /. "concurrentJobs") config configConcurrentJobs
#endif
#if MIN_VERSION_hspec_core(2,4,0)
    configIgnoreConfigFile <- getFromConfigWithDefault (key /. "ignoreConfigFile") config configIgnoreConfigFile
    configPrintCpuTime <- getFromConfigWithDefault (key /. "printCpuTime") config configPrintCpuTime
    configDiff <- getFromConfigWithDefault (key /. "diff") config configDiff
#endif
#if MIN_VERSION_hspec_core(2,4,2)
    configFailureReport <- getFromConfigWithDefault (key /. "failureReport") config configFailureReport
#endif
#if MIN_VERSION_hspec_core(2,7,0)
    configFocusedOnly <- getFromConfigWithDefault (key /. "focusedOnly") config configFocusedOnly
    configFailOnFocused <- getFromConfigWithDefault (key /. "failOnFocused") config configFailOnFocused
#endif
    pure Hspec.Config{..}
