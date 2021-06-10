# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]

### Added

* More strict parser:
  - Disallow invalid keys with a clear error message
  - Disallow using objects or arrays in the `_self` special key
* Implement the explain interfaces for `DhallSource`

## [v1.1.0.0] 2021-03-01

### Changed

* Rename `fromFilePath` to `fromFilePath'`.
* Define a new `fromFilePath` whose type is `FilePath -> SourceCreator` instaed of `FilePath -> IO Source`.

## [v1.0.0.0] 2020-12-29

First release

[Unreleased]: https://github.com/ludat/conferer/compare/conferer-dhall_v1.1.0.0...HEAD
[v1.1.0.0]: https://github.com/ludat/conferer/compare/conferer-dhall_v1.0.0.0...conferer-dhall_v1.1.0.0
[v1.0.0.0]: https://github.com/ludat/conferer/compare/v0.0.0.0...conferer-dhall_v1.0.0.0
