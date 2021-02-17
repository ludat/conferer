# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]

### Changed

* Rename `fromFilePath` to `fromFilePath'`.
* Define a new `fromFilePath` whose type is `FilePath -> SourceCreator` instaed of `FilePath -> IO Source`.
* Validate that the json value doesn't contain any wrong key name (only lowercase ascii and numbers
  are accepted) and throw otherwise.
* Even if invalid key names get through, ignore them in the source.


## [v1.0.0.0] 2020-12-29

First release

[Unreleased]: https://github.com/ludat/conferer/compare/conferer-aeson_v1.0.0.0...HEAD
[v1.0.0.0]: https://github.com/ludat/conferer/compare/v0.0.0.0...conferer-aeson_v1.0.0.0
