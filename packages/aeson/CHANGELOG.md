# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]

### Changed

* More strict parser for the json value:
  - Disallow invalid keys with a clear error message
  - Disallow using objects or arrays in the `_self` special key
* Accept aeson 2.2

## [v1.1.0.2] 2021-12-03

### Changed

* Allow using aeson-2.0

## [v1.1.0.1] 2021-03-14

### Changed

* Make the `keys` special key always return sorted keys (because of the change from hashable-1.3.1.0)

## [v1.1.0.0] 2021-03-01

### Changed

* Rename `fromFilePath` to `fromFilePath'`.
* Define a new `fromFilePath` whose type is `FilePath -> SourceCreator` instaed of `FilePath -> IO Source`.
* Validate that the json value doesn't contain any wrong key name (only lowercase ascii and numbers
  are accepted) and throw otherwise.
* Treat the special key `_self` as explained in the docs to allow nesting keys that's used elsewhere.
* Even if invalid key names get through, ignore them in the source.


## [v1.0.0.0] 2020-12-29

First release

[Unreleased]: https://github.com/ludat/conferer/compare/conferer-aeson_v1.1.0.1...HEAD
[v1.1.0.2]: https://github.com/ludat/conferer/compare/conferer-aeson_v1.1.0.1...conferer-aeson_v1.1.0.2
[v1.1.0.1]: https://github.com/ludat/conferer/compare/conferer-aeson_v1.1.0.0...conferer-aeson_v1.1.0.1
[v1.1.0.0]: https://github.com/ludat/conferer/compare/conferer-aeson_v1.0.0.0...conferer-aeson_v1.1.0.0
[v1.0.0.0]: https://github.com/ludat/conferer/compare/v0.0.0.0...conferer-aeson_v1.0.0.0
