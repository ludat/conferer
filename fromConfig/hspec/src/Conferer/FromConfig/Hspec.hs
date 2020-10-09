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

import Conferer.Types
import Conferer.FromConfig.Basics

import Data.Text (toLower)

import qualified Test.Hspec.Core.Runner as Hspec
import qualified Test.Hspec.Core.Formatters as Hspec

instance FromConfig Hspec.ColorMode where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig =
    fetchFromConfigWith
    (\t -> case t of
             "ColorAuto" -> Just Hspec.ColorAuto
             "ColorNever" -> Just Hspec.ColorNever
             "ColorAlways" -> Just Hspec.ColorAlways
             _ -> Nothing
    )

instance FromConfig Hspec.Formatter where
  updateFromConfig = updateAllAtOnceUsingFetch
  fetchFromConfig =
    fetchFromConfigWith
    (\t -> case toLower t of
             "silent" -> Just Hspec.silent
             "specdoc" -> Just Hspec.specdoc
             "progress" -> Just Hspec.progress
             "failed_examples" -> Just Hspec.failed_examples
             _ -> Nothing
    )

instance DefaultConfig Hspec.Config where
  configDef = Hspec.defaultConfig

instance FromConfig Hspec.Config where
  fetchFromConfig _key _config = return Nothing
  updateFromConfig k config original = do
    pure original
      >>= findKeyAndApplyConfig config k "dryRun" Hspec.configDryRun (\v c -> c { Hspec.configDryRun = v })
      >>= findKeyAndApplyConfig config k "fastFail" Hspec.configFastFail (\v c -> c { Hspec.configFastFail = v })
      >>= findKeyAndApplyConfig config k "rerun" Hspec.configRerun (\v c -> c { Hspec.configRerun = v })
      >>= findKeyAndApplyConfig config k "quickCheckMaxSuccess" Hspec.configQuickCheckMaxSuccess (\v c -> c { Hspec.configQuickCheckMaxSuccess = v })
      >>= findKeyAndApplyConfig config k "quickCheckMaxDiscardRatio" Hspec.configQuickCheckMaxDiscardRatio (\v c -> c { Hspec.configQuickCheckMaxDiscardRatio = v })
      >>= findKeyAndApplyConfig config k "quickCheckMaxSize" Hspec.configQuickCheckMaxSize (\v c -> c { Hspec.configQuickCheckMaxSize = v })
      >>= findKeyAndApplyConfig config k "quickCheckSeed" Hspec.configQuickCheckSeed (\v c -> c { Hspec.configQuickCheckSeed = v })
      >>= findKeyAndApplyConfig config k "smallCheckDepth" Hspec.configSmallCheckDepth (\v c -> c { Hspec.configSmallCheckDepth = v })
      >>= findKeyAndApplyConfig config k "colorMode" Hspec.configColorMode (\v c -> c { Hspec.configColorMode = v })
      >>= findKeyAndApplyConfig config k "htmlOutput" Hspec.configHtmlOutput (\v c -> c { Hspec.configHtmlOutput = v })
      >>= findKeyAndApplyConfig config k "formatter" Hspec.configFormatter (\v c -> c { Hspec.configFormatter = v })
#if MIN_VERSION_hspec_core(2,1,1)
      --  We ignore configuration of type function that don't have defaults provided by the lib
      -- >>= findKeyAndApplyConfig config k "skip-predicate" (\v c -> c { Hspec.configSkipPredicate = v })
#endif
#if MIN_VERSION_hspec_core(2,1,9)
      >>= findKeyAndApplyConfig config k "concurrentJobs" Hspec.configConcurrentJobs (\v c -> c { Hspec.configConcurrentJobs = v })
#endif
#if MIN_VERSION_hspec_core(2,4,0)
      >>= findKeyAndApplyConfig config k "ignoreConfigFile" Hspec.configIgnoreConfigFile (\v c -> c { Hspec.configIgnoreConfigFile = v })
      >>= findKeyAndApplyConfig config k "printCpuTime" Hspec.configPrintCpuTime (\v c -> c { Hspec.configPrintCpuTime = v })
      >>= findKeyAndApplyConfig config k "diff" Hspec.configDiff (\v c -> c { Hspec.configDiff = v })
#endif
#if MIN_VERSION_hspec_core(2,4,2)
      >>= findKeyAndApplyConfig config k "failureReport" Hspec.configFailureReport (\v c -> c { Hspec.configFailureReport = v })
#endif
#if MIN_VERSION_hspec_core(2,7,0)
      >>= findKeyAndApplyConfig config k "focusedOnly" Hspec.configFocusedOnly (\v c -> c { Hspec.configFocusedOnly = v })
      >>= findKeyAndApplyConfig config k "failOnFocused" Hspec.configFailOnFocused (\v c -> c { Hspec.configFailOnFocused = v })
#endif
      >>= return

