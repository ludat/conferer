# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]

### Added

* Added new functions `getKeyFromSources` and `getKeyFromDefaults`
* Change structure of `KeyLookupResult` to keep invariants of `getKeyFromSources` `getKeyFromDefaults`
* Added `asTopLevel` to display pretty errors and use `exitFailure` upon errors
* Fake `Show` and `Eq` instances for `Config`
* Reasonable implementations for the new `explain*` function on `Source` for the default sources
* `TestSource` which is like `InMemorySource` but throws when asking to explain keys (should be only
  used for testing)

### Removed

* Removed `fetchFromDefaults` and `fetchRequiredFromDefaults` in favor of `getKeyFromDefaults`

### Changed

* `listSubkeys` now ignores the defaults (since without a type parameter we can't guarantee that a value
    will always be found)
* `FoundInSources` now contains the index of the source it was found on
* `FromConfig` instances:
  - `File` throw with a `File` type on root key instead of string on every subkey (to improve error messages)
  - `List` now throws more specific errors (like `String` on `keys` instead of `[a]` on `.`) to
    report more actionable errors
  - `Bool` now accepts more values as true: `t`, `y`, `yes`, `1` and as false: `f`, `n`, `no`, `0`
* Moved exceptions related code from `FromConfig/Internal.hs` to `FromConfig/Internal/Types.hs`
* `explainSettedKey` and `explainMissingKey` to the `Source` typeclass to give better explanations
  of missing keys and setted keys
* `NullSource` and `InMemorySource` now accept one or more functions to implement the explain
  function from `Source`

## [v1.1.0.0] - 2021-03-01

### Changed

* Rename `fromFilePath` to `fromFilePath'`.
* Define a new `fromFilePath` whose type is `FilePath -> SourceCreator` instaed of `FilePath -> IO Source`.
* (Internal) Remove the mismatched type exception since it's not actionable by the user
* (Internal) Use a list for default values so that many different defaults are available,
  possible point of extension in the future.
* Constraint valid key names to lowercase ascii and numbers (previously some non ascii characters were allowed)

### Added

* `isValidKeyFragment` and `isKeyCharacter` to validate `Key`s
* Added an overriding method based on type and key (details in the docs)

## [v1.0.0.1] - 2021-01-17

### Fixed

* In the `File`'s `FromConfig` instance, if the default is present and it's type
is `File`, it throws, which doesn't follow the rest of the library.

### Added

* Add `mkConfig'` which allows creating a config by passing a list of defaults and
a list of source creators.
* Add `addSources`, which allows to add several sources to a config.
* Define the type `Defaults`, which is a list of associations from `Key` to
`Dynamic`.

## [v1.0.0.0] - 2020-12-29

First release

[Unreleased]: https://github.com/ludat/conferer/compare/conferer_v1.1.0.0...HEAD
[v1.1.0.0]: https://github.com/ludat/conferer/releases/tag/conferer_v1.0.0.1...conferer_v1.1.0.0
[v1.0.0.1]: https://github.com/ludat/conferer/releases/tag/conferer_v1.0.0.0...conferer_v1.0.0.1
[v1.0.0.0]: https://github.com/ludat/conferer/releases/tag/v0.0.0.0...conferer_v1.0.0.0
