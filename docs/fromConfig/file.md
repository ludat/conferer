---
id: file
title: File FromConfig
---

## File FromConfig

This FromConfig instance provides support for parsing values of type `File`, which is a newtype wrapping `Filepath`.


## Filepath and overrides

This instance can read a filepath as a File, but if any more specific value is given that value will
be used instead.

So if you have `FILE_EXTENSION=jpg` and cli arg `--file=someFile.png`, the file will be read as `someFile.jpg` since
`"file.extension"` is more specific than `"file"`. This may change in the future but that's the current
behavior, if enough people find this confusing we may change it.

### `extension :: String`

The file extension.

Example: `md` in `docs/fromConfig/file.md`.

### `dirname :: String`

The path without the filename.

Example: `docs/fromConfig` in `docs/fromConfig/file.md`.

### `basename :: String`

The filename without the extension.

Example: `file` in `docs/fromConfig/file.md`.

### `filename :: String`

The filename (basename + extension).

Example: `file.md` in `docs/fromConfig/file.md`.