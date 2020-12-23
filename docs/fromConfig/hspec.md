---
id: hspec
title: Hspec FromConfig
---

## Hspec FromConfig

This FromConfig instance provider support for parsing [Hspec's Config](https://hackage.haskell.org/package/hspec-core-2.7.4/docs/Test-Hspec-Core-Runner.html#t:Config).

## Configurable values

### `ignoreConfigFile :: Bool`

Ignore .hspec config files

Default: `False`

### `dryRun :: Bool`

Do a dry run instead of actually running the tests

Default: `False`

### `focusedOnly :: Bool`

Run tests only if they are focused

Default: `False`

### `failOnFocused :: Bool`

Fail if any test is focused

Default: `False`

### `printCpuTime :: Bool`

Print the cpu runtime at the end of the tests

Default: `False`

### `fastFail :: Bool`

Fail whole suite as soon as any test fails

Default: `False`

### `randomize :: Bool`

Randomize execution order

Default: `False`

### `failureReport :: Maybe FilePath`

Specify path for hspec to report errors

Default: `Nothing`

### `rerun :: Bool`

rerun all examples that failed in the previous test run

Default: `False`

### `rerunAllOnSuccess :: Bool`

run the whole test suite after a previously failing rerun
succeeds for the first time

Default: `False`

### `filterPredicate :: Maybe (Path -> Bool)` (NOT CONFIGURABLE BY USER YET)

A predicate that is used to filter the spec before it is run.
Only examples that satisfy the predicate are run.

Default: `Nothing`

### `skipPredicate :: Maybe (Path -> Bool)` (NOT CONFIGURABLE BY USER YET)


A predicate that is used to filter the spec before it is run.
Only examples that satisfy the predicate are run.

Default: ``

### `quickCheckSeed :: Maybe Integer`

The seed for quickcheck. Nothing means it's random

Default: `Nothing`

### `quickCheckMaxSuccess :: Maybe Int`

Maximum successful runs to quit. Nothing means no limit

Default: `Nothing`

### `quickCheckMaxDiscardRatio :: Maybe Int`

Maximum discard ration for quickcheck. Nothing means no limit.

Default: `Nothing`

### `quickCheckMaxSize :: Maybe Int`

Maximum check size for quickcheck. Nothing means no limit.

Default: `Nothing`

### `smallCheckDepth :: Int`

Depth for smallcheck.

Default: `5` extracted from smallcheck's defaults

### `colorMode :: ColorMode`

Whether tests results should be printed with colors,
may be one of the following:

* `auto`: Use colors only when terminal emulator supports it
* `always`: Always use colors
* `never`: Never use colors

Default: `auto`

### `diff :: Bool`

Show colorized diffs.

Default: `True`

### `formatter :: Maybe Formatter` (NOT CONFIGURABLE BY THE USER)

Formatter for test results, Nothing means using the default
formatter.

Default: `Nothing`

### `htmlOutput :: Bool`

Use html format as output.

Default: `False`

### `outputFile :: Either Handle FilePath`

Where should the hspec results end up.

Default: `Left stdout`

### `concurrentJobs :: Maybe Int`

Number of concurrent jobs to use, Nothing means numbers of cpus

Default: `Nothing`