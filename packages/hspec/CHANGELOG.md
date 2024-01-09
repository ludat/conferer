# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]
### Fixed
### Changed
### Added

## [v1.1.1.0] 2024-01-08

### Fixed

* Added all defaults for now the FromConfig instance actually works

### Changed

* `"checks"` is a valid value for `formatter` key (with `hspec >= 2.7.10`)
* `failed-examples` is a valid value for `formatter`

### Added

* `"randomize"` key is used for configuring `configRandomize`
* `"printSlowItems"` key is used for configuring `configPrintSlowItems`
* `"quickCheckMaxShrinks"` key is used for configuring `configQuickCheckMaxShrinks`
* `"times"` key is used for configuring `configTimes`
* `"format"` key is used for configuring `configFormat`
* `"failOnEmptyDescription"` key is used for configuring `configFailOnEmptyDescription`
* `"expertMode"` key is used for configuring `configExpertMode`
* `"foformatException` key is used for configuring `configFormatException`

## [v1.1.0.0] 2021-03-07

### Changed

* Update lib to match `conferer`

## [v1.0.0.0] 2020-12-29

First release

[Unreleased]: https://github.com/ludat/conferer/compare/conferer-hspec_v1.1.0.0...HEAD
[v1.1.0.0]: https://github.com/ludat/conferer/compare/conferer-hspec_v1.0.0.0...conferer-hspec_v1.1.0.0
[v1.0.0.0]: https://github.com/ludat/conferer/compare/v0.0.0.0...conferer-hspec_v1.0.0.0
