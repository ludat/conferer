# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP](https://pvp.haskell.org/).

## [Unreleased]

### Changed

* Rename `fromFilePath` to `fromFilePath'`.
* Define a new `fromFilePath` whose type is `FilePath -> SourceCreator` instaed of `FilePath -> IO Source`.

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

[Unreleased]: https://github.com/ludat/conferer/compare/conferer_v1.0.0.0...HEAD
[v1.0.0.0]: https://github.com/ludat/conferer/releases/tag/v0.0.0.0...conferer_v1.0.0.0
[v1.0.0.1]: https://github.com/ludat/conferer/releases/tag/conferer_v1.0.0.0...conferer_v1.0.0.1
