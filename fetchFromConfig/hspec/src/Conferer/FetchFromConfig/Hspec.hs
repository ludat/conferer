{-# LANGUAGE CPP #-}
module Conferer.FetchFromConfig.Hspec
  (
  -- * How to use this
  -- | FetchFromConfig instance for snap server configuration
  --
  -- @
  -- import Conferer
  -- import Conferer.FetchFromConfig.Snap ()
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

import Conferer.Core
import Conferer.Types
import Conferer.FetchFromConfig.Basics

import Data.Either (rights)
import Data.String (fromString)
import Data.Text (Text, unpack, toLower)

import qualified Test.Hspec.Core.Runner as Hspec
import qualified Test.Hspec.Core.Formatters as Hspec

instance FetchFromConfig Hspec.ColorMode where
  fetch =
    fetchFromConfigWith
    (\t -> case t of
             "ColorAuto" -> Just Hspec.ColorAuto
             "ColorNever" -> Just Hspec.ColorNever
             "ColorAlways" -> Just Hspec.ColorAlways
             _ -> Nothing
    )

instance FetchFromConfig Hspec.Formatter where
  fetch =
    fetchFromConfigWith
    (\t -> case toLower t of
             "silent" -> Just Hspec.silent
             "specdoc" -> Just Hspec.specdoc
             "progress" -> Just Hspec.progress
             "failed_examples" -> Just Hspec.failed_examples
             _ -> Nothing
    )

instance FetchFromConfig Hspec.Config where
  fetch k config = do
    pure (Right Hspec.defaultConfig)
      >>= findKeyAndApplyConfig config k "dry-run" (\v c -> c { Hspec.configDryRun = v })
      >>= findKeyAndApplyConfig config k "fast-fail" (\v c -> c { Hspec.configFastFail = v })
      >>= findKeyAndApplyConfig config k "rerun" (\v c -> c { Hspec.configRerun = v })
      >>= findKeyAndApplyConfig config k "quickCheck-max-success" (\v c -> c { Hspec.configQuickCheckMaxSuccess = v })
      >>= findKeyAndApplyConfig config k "quickCheck-max-discard-ratio" (\v c -> c { Hspec.configQuickCheckMaxDiscardRatio = v })
      >>= findKeyAndApplyConfig config k "quickCheck-max-size" (\v c -> c { Hspec.configQuickCheckMaxSize = v })
      >>= findKeyAndApplyConfig config k "quickCheck-seed" (\v c -> c { Hspec.configQuickCheckSeed = v })
      >>= findKeyAndApplyConfig config k "smallCheck-depth" (\v c -> c { Hspec.configSmallCheckDepth = v })
      >>= findKeyAndApplyConfig config k "color-mode" (\v c -> c { Hspec.configColorMode = v })
      >>= findKeyAndApplyConfig config k "html-output" (\v c -> c { Hspec.configHtmlOutput = v })
      >>= findKeyAndApplyConfig config k "formatter" (\v c -> c { Hspec.configFormatter = v })
#if MIN_VERSION_hspec_core(2,1,1)
      --  We ignore configuration of type function that don't have defaults provided by the lib
      -- >>= findKeyAndApplyConfig config k "skip-predicate" (\v c -> c { Hspec.configSkipPredicate = v })
#endif
#if MIN_VERSION_hspec_core(2,1,9)
      >>= findKeyAndApplyConfig config k "concurrent-jobs" (\v c -> c { Hspec.configConcurrentJobs = v })
#endif
#if MIN_VERSION_hspec_core(2,4,0)
      >>= findKeyAndApplyConfig config k "ignore-config-file" (\v c -> c { Hspec.configIgnoreConfigFile = v })
      >>= findKeyAndApplyConfig config k "print-cpu-time" (\v c -> c { Hspec.configPrintCpuTime = v })
      >>= findKeyAndApplyConfig config k "diff" (\v c -> c { Hspec.configDiff = v })
#endif
#if MIN_VERSION_hspec_core(2,4,2)
      >>= findKeyAndApplyConfig config k "failure-report" (\v c -> c { Hspec.configFailureReport = v })
#endif
#if MIN_VERSION_hspec_core(2,7,0)
      >>= findKeyAndApplyConfig config k "focused-only" (\v c -> c { Hspec.configFocusedOnly = v })
      >>= findKeyAndApplyConfig config k "fail-on-focused" (\v c -> c { Hspec.configFailOnFocused = v })
#endif

