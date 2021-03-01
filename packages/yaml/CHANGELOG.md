# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]

Nothing

## [v1.1.0.0] 2021-03-01

### Changed

* Rename `fromFilePath` to `fromFilePath'`.
* Define a new `fromFilePath` whose type is `FilePath -> SourceCreator` instaed of `FilePath -> IO Source`.

## [v1.0.0.0] 2020-12-29

First release

[Unreleased]: https://github.com/ludat/conferer/compare/conferer-yaml_v1.1.0.0...HEAD
[v1.1.0.0]: https://github.com/ludat/conferer/compare/conferer-yaml_v1.0.0.0...conferer-yaml_v1.1.0.0
[v1.0.0.0]: https://github.com/ludat/conferer/compare/v0.0.0.0...conferer-yaml_v1.0.0.0
